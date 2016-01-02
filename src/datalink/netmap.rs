// Copyright (c) 2015, 2015 Robert Clipsham <robert@octarineparrot.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![allow(bad_style)]

extern crate netmap_sys;
extern crate libc;

use self::netmap_sys::netmap_user::{NETMAP_BUF, NETMAP_FD, NETMAP_TXRING, nm_close, nm_desc,
                                    nm_nextpkt, nm_open, nm_pkthdr, nm_ring_next};
use self::netmap_sys::netmap::{netmap_slot, nm_ring_empty};

use std::ffi::CString;
use std::path::Path;
use std::fs::File;
use std::io;
use std::io::Read;
use std::mem;
use std::num;
use std::ptr;
use std::raw;
use std::sync::Arc;

use datalink::{DataLinkChannelIterator, DataLinkChannelType, DataLinkReceiver, DataLinkSender};
use packet::Packet;
use packet::ethernet::{EthernetPacket, MutableEthernetPacket};
use util::NetworkInterface;

#[cfg(any(target_os = "linux", target_os = "freebsd"))]
#[repr(C)]
struct pollfd {
    fd: libc::c_int,
    events: libc::c_short,
    revents: libc::c_short,
}

#[cfg(any(target_os = "linux", target_os = "freebsd"))]
const POLLIN: libc::c_short = 0x0001;
#[cfg(any(target_os = "linux", target_os = "freebsd"))]
const POLLOUT: libc::c_short = 0x0004;

#[cfg(target_os = "freebsd")]
type nfds_t = libc::c_uint;
#[cfg(target_os = "linux")]
type nfds_t = libc::c_ulong;

extern {
    fn poll(fds: *mut pollfd, nfds: nfds_t, timeout: libc::c_int) -> libc::c_int;
}

struct NmDesc {
    desc: *mut nm_desc,
    buf_size: libc::c_uint,
}

unsafe impl Send for NmDesc {}
unsafe impl Sync for NmDesc {}

impl NmDesc {
    fn new(iface: &NetworkInterface) -> io::Result<NmDesc> {
        let ifname = CString::new(("netmap:".to_string() + &iface.name[..]).as_bytes());
        let desc = unsafe { nm_open(ifname.unwrap().as_ptr(), ptr::null(), 0, ptr::null()) };

        if desc.is_null() {
            Err(io::Error::last_os_error())
        } else {
            let mut f = try!(File::open(&Path::new("/sys/module/netmap/parameters/buf_size")));
            let mut num_str = String::new();
            try!(f.read_to_string(&mut num_str));
            let buf_size = num_str.trim_right().parse().unwrap();

            Ok(NmDesc {
                desc: desc,
                buf_size: buf_size,
            })
        }
    }
}

impl Drop for NmDesc {
    fn drop(&mut self) {
        unsafe {
            nm_close(self.desc);
        }
    }
}

#[inline]
pub fn datalink_channel(network_interface: &NetworkInterface,
                        _write_buffer_size: usize,
                        _read_buffer_size: usize,
                        _channel_type: DataLinkChannelType)
    -> io::Result<(Box<DataLinkSender>, Box<DataLinkReceiver>)> {
    // FIXME probably want one for each of send/recv
    let desc = NmDesc::new(network_interface);
    match desc {
        Ok(desc) => {
            let arc = Arc::new(desc);

            Ok((Box::new(DataLinkSenderImpl { desc: arc.clone() }),
                Box::new(DataLinkReceiverImpl { desc: arc })))
        }
        Err(e) => Err(e),
    }
}

pub struct DataLinkSenderImpl {
    desc: Arc<NmDesc>,
}

impl DataLinkSender for DataLinkSenderImpl {
    #[inline]
    fn build_and_send(&mut self,
                      num_packets: usize,
                      packet_size: usize,
                      func: &mut FnMut(MutableEthernetPacket))
        -> Option<io::Result<()>> {
        assert!(packet_size <= self.desc.buf_size as usize);
        let desc = self.desc.desc;
        let mut fds = pollfd {
            fd: unsafe { NETMAP_FD(desc) },
            events: POLLOUT,
            revents: 0,
        };
        let mut packet_idx = 0usize;
        while packet_idx < num_packets {
            unsafe {
                if poll(&mut fds, 1, -1) < 0 {
                    return Some(Err(io::Error::last_os_error()));
                }
                let ring = NETMAP_TXRING((*desc).nifp, 0);
                while !nm_ring_empty(ring) && packet_idx < num_packets {
                    let i = (*ring).cur;
                    let slot_ptr: *mut netmap_slot = mem::transmute(&mut (*ring).slot);
                    let buf = NETMAP_BUF(ring, (*slot_ptr.offset(i as isize)).buf_idx as isize);
                    let slice = raw::Slice {
                        data: buf,
                        len: packet_size,
                    };
                    let meh = MutableEthernetPacket::new(mem::transmute(slice)).unwrap();
                    (*slot_ptr.offset(i as isize)).len = packet_size as u16;
                    func(meh);
                    let next = nm_ring_next(ring, i);
                    (*ring).head = next;
                    (*ring).cur = next;
                    packet_idx += 1;
                }
            }
        }

        Some(Ok(()))
    }

    #[inline]
    fn send_to(&mut self,
               packet: &EthernetPacket,
               _dst: Option<NetworkInterface>)
        -> Option<io::Result<()>> {
        use packet::MutablePacket;
        self.build_and_send(1,
                            packet.packet().len(),
                            &mut |mut eh: MutableEthernetPacket| {
                                eh.clone_from(packet);
                            })
    }
}

pub struct DataLinkReceiverImpl {
    desc: Arc<NmDesc>,
}

impl DataLinkReceiver for DataLinkReceiverImpl {
    // FIXME Layer 3
    fn iter<'a>(&'a mut self) -> Box<DataLinkChannelIterator + 'a> {
        Box::new(DataLinkChannelIteratorImpl { pc: self })
    }
}

pub struct DataLinkChannelIteratorImpl<'a> {
    pc: &'a mut DataLinkReceiverImpl,
}

impl<'a> DataLinkChannelIterator<'a> for DataLinkChannelIteratorImpl<'a> {
    fn next<'c>(&'c mut self) -> io::Result<EthernetPacket<'c>> {
        let desc = self.pc.desc.desc;
        let mut h: nm_pkthdr = unsafe { mem::uninitialized() };
        let mut buf = unsafe { nm_nextpkt(desc, &mut h) };
        if buf.is_null() {
            let mut fds = pollfd {
                fd: unsafe { NETMAP_FD(desc) },
                events: POLLIN,
                revents: 0,
            };
            if unsafe { poll(&mut fds, 1, -1) } < 0 {
                return Err(io::Error::last_os_error());
            }
            buf = unsafe { nm_nextpkt(desc, &mut h) };
        }
        Ok(EthernetPacket::new(unsafe {
            mem::transmute(raw::Slice {
                data: buf,
                len: h.len as usize,
            })
        }).unwrap())
    }
}
