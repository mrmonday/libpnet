// Copyright (c) 2014 Robert Clipsham <robert@octarineparrot.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Provides interfaces for interacting with packets and headers

#![macro_escape]

/// Represents a generic network packet
pub trait Packet {
    /// Retreive the underlying buffer for the packet
    fn packet<'p>(&'p self) -> &'p [u8];

    /// Retreive the payload for the packet
    fn payload<'p>(&'p self) -> &'p [u8];
}

/// Represents a generic, mutable, network packet
pub trait MutablePacket {
    /// Retreive the underlying, mutable, buffer for the packet
    fn mut_packet<'p>(&'p mut self) -> &'p mut [u8];

    /// Retreive the mutable payload for the packet
    fn mut_payload<'p>(&'p mut self) -> &'p mut [u8];

    /// Initialize this packet by cloning another
    fn clone_from<'p, T : Packet>(&'p mut self, other: T) {
        use std::slice::bytes::copy_memory;
        copy_memory(self.mut_packet(), other.packet())
    }
}

/// FIXME
pub struct PacketBuilder<'p> {
    packet: &'p mut [u8],
    index: uint
}

impl<'p> PacketBuilder<'p> {
    /// FIXME
    pub fn new(buffer: &'p mut [u8]) -> PacketBuilder<'p> {
        PacketBuilder { packet: buffer, index: 0 }
    }

    /// FIXME
    pub fn append<T : ToPacket<'p, U>, U : Packet>(&'p mut self, packet: T) -> &'p mut PacketBuilder {
        let index = self.index;
        packet.to_packet(self.packet.mut_slice_from(index));
        self.index += packet.header_len();

        self
    }
}

impl<'p> Packet for PacketBuilder<'p> {
    fn packet<'p>(&'p self) -> &'p [u8] { self.packet }
    fn payload<'p>(&'p self) -> &'p [u8] { self.packet.slice_from(self.index) }
}

impl<'p> MutablePacket for PacketBuilder<'p> {
    fn mut_packet<'p>(&'p mut self) -> &'p mut [u8] { self.packet }
    fn mut_payload<'p>(&'p mut self) -> &'p mut [u8] { self.packet.mut_slice_from(self.index) }
}

/// FIXME
pub trait ToPacket<'p, T : Packet> {
    ///
    fn to_packet(&self, buffer: &mut [u8]);

    ///
    fn header_len(&self) -> uint;
}

/// FIXME
pub trait FromPacket<T> {
    ///
    fn from_packet(&self) -> T;
}

/// Provides a faux-for loop for packet channels.
///
/// Usage:
/// ```
/// pfor!(packet in some_iterator {
///     /* Do something with packet */
/// })
/// ```
/// It may also handle errors:
/// ```rust
/// pfor!(packet in some_iterator {
///     /* Do something with packet */
/// } on Err(e) {
///     fail!("An error occured while receiving packets: {}", e);
/// })
/// ```
#[macro_export]
pub macro_rules! pfor (
    ($var:pat in $iter:expr $body:block on Err($err:pat) $err_body:block) => {{
        let mut iter = $iter;
        loop {
            let val = iter.next();
            match val {
                Ok($var) => $body,
                Err($err) => $err_body
            }
        }
    }};

    ($var:pat in $iter:expr $body:block) => {
        pfor!($var in $iter $body on Err(_) {})
    }
)

pub mod ethernet;
pub mod ip;
pub mod ipv4;
pub mod ipv6;
pub mod udp;

