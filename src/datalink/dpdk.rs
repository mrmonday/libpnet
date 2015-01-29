// Copyright (c) 2015 Robert Clipsham <robert@octarineparrot.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![allow(bad_style)]
#![unstable]

extern crate "rust-dpdk" as dpdk;
extern crate libc;

use libc::{c_int, c_uint, c_ulong, c_short};
//
//use std::ffi::CString;
//use std::path::Path;
//use std::old_io::fs::File;
use std::old_io::{IoResult, IoError};
//use std::mem;
//use std::num;
//use std::ptr;
//use std::raw;
//use std::sync::Arc;
//
use datalink::DataLinkChannelType;
use packet::Packet;
use packet::ethernet::{EthernetHeader, MutableEthernetHeader};
use util::{NetworkInterface};

#[cfg(target_os = "linux")]
#[repr(C)]
struct pollfd {
    fd: c_int,
    events: c_short,
    revents: c_short
}

#[cfg(target_os = "linux")]
const POLLIN: c_short = 0x0001;
#[cfg(target_os = "linux")]
const POLLOUT: c_short = 0x0004;

type nfds_t = c_ulong;

extern {
    fn poll(fds: *mut pollfd, nfds: nfds_t, timeout: c_int) -> c_int;
}

//struct NmDesc {
//    desc: *mut nm_desc,
//    buf_size: c_uint,
//}
//
//impl NmDesc {
//    fn new(iface: &NetworkInterface) -> IoResult<NmDesc> {
//    }
//}
//
//impl Drop for NmDesc {
//    fn drop(&mut self) {
//        unsafe {
//        }
//    }
//}


pub struct DataLinkSenderImpl {
    _dummy: u8
}

impl DataLinkSenderImpl {
    pub fn build_and_send<F>(&mut self, num_packets: usize, packet_size: usize,
                          func: &mut F) -> Option<IoResult<()>>
        where F : FnMut(MutableEthernetHeader)
    {
        unimplemented!();
    }

    pub fn send_to(&mut self, packet: EthernetHeader, _dst: Option<NetworkInterface>)
        -> Option<IoResult<()>> {
        use packet::MutablePacket;
        self.build_and_send(1, packet.packet().len(), &mut |&mut:mut eh: MutableEthernetHeader| {
            eh.clone_from(packet);
        })
    }
}

pub struct DataLinkReceiverImpl {
    _dummy: u8
}

impl DataLinkReceiverImpl {
    // FIXME Layer 3
    pub fn iter<'a>(&'a mut self) -> DataLinkChannelIteratorImpl<'a> {
        DataLinkChannelIteratorImpl {
            pc: self,
        }
    }
}

pub fn datalink_channel(network_interface: &NetworkInterface,
                        _write_buffer_size: usize,
                        _read_buffer_size: usize,
                        _channel_type: DataLinkChannelType)
    -> IoResult<(DataLinkSenderImpl, DataLinkReceiverImpl)> {
    unimplemented!();
}

pub struct DataLinkChannelIteratorImpl<'a> {
    pc: &'a mut DataLinkReceiverImpl,
}

impl<'a> DataLinkChannelIteratorImpl<'a> {
    pub fn next<'c>(&'c mut self) -> IoResult<EthernetHeader<'c>> {
        unimplemented!();
    }
}

