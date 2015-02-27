// Copyright (c) 2015 Robert Clipsham <robert@octarineparrot.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//#![warn(missing_docs)]

#![feature(core, plugin_registrar, rustc_private)]

extern crate syntax;
extern crate regex;
extern crate rustc;

use regex::Regex;

use rustc::plugin::Registry;

use std::fmt;
use std::num::SignedInt;

use syntax::ast;
use syntax::codemap::{Span, Spanned};
use syntax::owned_slice::OwnedSlice;
use syntax::parse::token;
use syntax::ext::base::{ExtCtxt, Decorator};
use syntax::ext::build::AstBuilder;
use syntax::ext::quote::rt::ExtParseUtils;
use syntax::ptr::P;

#[derive(Debug, PartialEq, Eq)]
enum Endianness {
    Big,
    Little
}

#[derive(Copy, Debug, PartialEq, Eq)]
struct Operation {
    mask: u8,
    shiftl: u8,
    shiftr: u8,
}

impl fmt::Display for Operation {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let should_mask = self.mask != 0xFF;
        let shift = (self.shiftr as i16) - (self.shiftl as i16);

        let mask_str = if should_mask {
            format!("({{}} & 0x{})", fmt::radix(self.mask, 16))
        } else {
            "{}".to_string()
        };

        if shift == 0 {
            write!(fmt, "{}", mask_str)
        } else if shift < 0 {
            write!(fmt, "{} << {}", mask_str, shift.abs())
        } else {
            write!(fmt, "{} >> {}", mask_str, shift.abs())
        }
    }
}

#[test]
fn test_display_operation() {
    type Op = Operation;

    assert_eq!(Op { mask: 0b00001111, shiftl: 2, shiftr: 0 }.to_string(), "({} & 0xf) << 2");
    assert_eq!(Op { mask: 0b00001111, shiftl: 2, shiftr: 2 }.to_string(), "({} & 0xf)");
    assert_eq!(Op { mask: 0b00001111, shiftl: 0, shiftr: 2 }.to_string(), "({} & 0xf) >> 2");
    assert_eq!(Op { mask: 0b11111111, shiftl: 0, shiftr: 2 }.to_string(), "{} >> 2");
    assert_eq!(Op { mask: 0b11111111, shiftl: 3, shiftr: 1 }.to_string(), "{} << 2");
}

fn mask_high_bits(mut bits: u8) -> u8 {
    let mut mask = 0;
    while bits > 0 {
        mask = mask | (1 << bits);
        bits -= 1;
    }

    mask
}

/// Gets a mask to get bits_remaining bits from offset bits into a byte
/// If bits_remaining is > 8, it will be truncated as necessary
fn get_mask(offset: usize, bits_remaining: usize) -> (usize, u8) {
    println!("get_mask(offset={}, bits_remaining={})", offset, bits_remaining);
    fn bits_remaining_in_byte(offset: usize, bits_remaining: usize) -> usize {
        fn round_down(max_val: usize, val: usize) -> usize {
            if val > max_val {
                max_val
            } else {
                val
            }
        }
        if (bits_remaining / 8) >= 1 {
            8 - offset
        } else {
            round_down(8 - offset, bits_remaining)
        }
    }
    assert!(offset <= 7);
    let mut num_bits_to_mask = bits_remaining_in_byte(offset, bits_remaining);
    println!("num_bits_to_mask: {}", num_bits_to_mask);
    assert!(num_bits_to_mask <= 8 - offset);
    let mut mask = 0;
    while num_bits_to_mask > 0 {
        mask = mask | (0x80 >> (offset + num_bits_to_mask - 1));
        num_bits_to_mask -= 1;
    }

    (bits_remaining_in_byte(offset, bits_remaining), mask)
}

#[test]
fn test_get_mask() {
    assert_eq!(get_mask(0, 1), (1, 0b10000000));
    assert_eq!(get_mask(0, 2), (2, 0b11000000));
    assert_eq!(get_mask(0, 3), (3, 0b11100000));
    assert_eq!(get_mask(0, 4), (4, 0b11110000));
    assert_eq!(get_mask(0, 5), (5, 0b11111000));
    assert_eq!(get_mask(0, 6), (6, 0b11111100));
    assert_eq!(get_mask(0, 7), (7, 0b11111110));
    assert_eq!(get_mask(0, 8), (8, 0b11111111));
    assert_eq!(get_mask(0, 9), (8, 0b11111111));
    assert_eq!(get_mask(0, 100), (8, 0b11111111));

    assert_eq!(get_mask(1, 1), (1, 0b01000000));
    assert_eq!(get_mask(1, 2), (2, 0b01100000));
    assert_eq!(get_mask(1, 3), (3, 0b01110000));
    assert_eq!(get_mask(1, 4), (4, 0b01111000));
    assert_eq!(get_mask(1, 5), (5, 0b01111100));
    assert_eq!(get_mask(1, 6), (6, 0b01111110));
    assert_eq!(get_mask(1, 7), (7, 0b01111111));
    assert_eq!(get_mask(1, 8), (7, 0b01111111));
    assert_eq!(get_mask(1, 9), (7, 0b01111111));
    assert_eq!(get_mask(1, 100), (7, 0b01111111));

    assert_eq!(get_mask(5, 1), (1, 0b00000100));
    assert_eq!(get_mask(5, 2), (2, 0b00000110));
    assert_eq!(get_mask(5, 3), (3, 0b00000111));
    assert_eq!(get_mask(5, 4), (3, 0b00000111));
    assert_eq!(get_mask(5, 5), (3, 0b00000111));
    assert_eq!(get_mask(5, 6), (3, 0b00000111));
    assert_eq!(get_mask(5, 7), (3, 0b00000111));
    assert_eq!(get_mask(5, 8), (3, 0b00000111));
    assert_eq!(get_mask(5, 100), (3, 0b00000111));
}

fn get_shiftl(offset: usize, size: usize, byte_number: usize, num_bytes: usize) -> u8 {
    if num_bytes == 1 || byte_number + 1 == num_bytes {
        0
    } else {
        let base_shift = 8 - ((num_bytes * 8) - offset - size);
        let bytes_to_shift = num_bytes - byte_number - 2;

        (base_shift + (8 * bytes_to_shift)) as u8
    }
}

#[test]
fn test_get_shiftl() {
    assert_eq!(get_shiftl(0, 8, 0, 1), 0);
    assert_eq!(get_shiftl(0, 9, 0, 2), 1);
    assert_eq!(get_shiftl(0, 9, 1, 2), 0);
    assert_eq!(get_shiftl(0, 10, 0, 2), 2);
    assert_eq!(get_shiftl(0, 10, 1, 2), 0);
    assert_eq!(get_shiftl(0, 11, 0, 2), 3);
    assert_eq!(get_shiftl(0, 11, 1, 2), 0);

    assert_eq!(get_shiftl(1, 7, 0, 1), 0);
    assert_eq!(get_shiftl(1, 8, 0, 2), 1);
    assert_eq!(get_shiftl(1, 9, 0, 2), 2);
    assert_eq!(get_shiftl(1, 9, 1, 2), 0);
    assert_eq!(get_shiftl(1, 10, 0, 2), 3);
    assert_eq!(get_shiftl(1, 10, 1, 2), 0);
    assert_eq!(get_shiftl(1, 11, 0, 2), 4);
    assert_eq!(get_shiftl(1, 11, 1, 2), 0);

    assert_eq!(get_shiftl(0, 35, 0, 5), 27);
    assert_eq!(get_shiftl(0, 35, 1, 5), 19);
    assert_eq!(get_shiftl(0, 35, 2, 5), 11);
    assert_eq!(get_shiftl(0, 35, 3, 5), 3);
    assert_eq!(get_shiftl(0, 35, 4, 5), 0);
}

fn get_shiftr(offset: usize, size: usize, byte_number: usize, num_bytes: usize) -> u8 {
    if byte_number + 1 == num_bytes {
        ((num_bytes * 8) - offset - size) as u8
    } else {
        0
    }
}

#[test]
fn test_get_shiftr() {
    assert_eq!(get_shiftr(0, 1, 0, 1), 7);
    assert_eq!(get_shiftr(0, 2, 0, 1), 6);
    assert_eq!(get_shiftr(0, 3, 0, 1), 5);
    assert_eq!(get_shiftr(0, 4, 0, 1), 4);
    assert_eq!(get_shiftr(0, 5, 0, 1), 3);
    assert_eq!(get_shiftr(0, 6, 0, 1), 2);
    assert_eq!(get_shiftr(0, 7, 0, 1), 1);
    assert_eq!(get_shiftr(0, 8, 0, 1), 0);
    assert_eq!(get_shiftr(0, 9, 0, 2), 0);
    assert_eq!(get_shiftr(0, 9, 1, 2), 7);

    assert_eq!(get_shiftr(1, 7, 0, 1), 0);
    assert_eq!(get_shiftr(1, 8, 0, 2), 0);
    assert_eq!(get_shiftr(1, 8, 1, 2), 7);
    assert_eq!(get_shiftr(1, 9, 0, 2), 0);
    assert_eq!(get_shiftr(1, 9, 1, 2), 6);
    assert_eq!(get_shiftr(1, 10, 0, 2), 0);
    assert_eq!(get_shiftr(1, 10, 1, 2), 5);
    assert_eq!(get_shiftr(1, 11, 0, 2), 0);
    assert_eq!(get_shiftr(1, 11, 1, 2), 4);


    assert_eq!(get_shiftr(0, 35, 0, 5), 0);
    assert_eq!(get_shiftr(0, 35, 1, 5), 0);
    assert_eq!(get_shiftr(0, 35, 2, 5), 0);
    assert_eq!(get_shiftr(0, 35, 3, 5), 0);
    assert_eq!(get_shiftr(0, 35, 4, 5), 5);
}

/// Given an offset (number of bits into a chunk of memory), retreive a list of operations to get
/// size bits.
///
/// Assumes big endian, and that each byte will be masked, then cast to the next power of two
/// greater than or equal to size bits before shifting. offset should be in the range [0, 7]
fn operations(offset: usize, size: usize) -> Option<Vec<Operation>> {
    if offset > 7 || size == 0 || size > 64 {
        return None;
    }

    let num_full_bytes = size / 8;
    let num_bytes = if offset > 0 || size % 8 != 0{
                        num_full_bytes + 1
                    } else {
                        num_full_bytes
                    };

    let mut current_offset = offset;
    let mut num_bits_remaining = size;
    let mut ops = Vec::with_capacity(num_bytes);
    for i in range(0, num_bytes) {
        let (consumed, mask) = get_mask(current_offset, num_bits_remaining);
        ops.push(Operation {
            mask: mask,
            shiftl: get_shiftl(offset, size, i, num_bytes),
            shiftr: get_shiftr(offset, size, i, num_bytes),
        });
        current_offset = 0;
        if num_bits_remaining >= consumed {
            num_bits_remaining -= consumed;
        }
    }

    Some(ops)
}

#[test]
fn operations_test() {
    type Op = Operation;
    assert_eq!(operations(0, 1).unwrap(), vec!(Op { mask: 0b10000000, shiftl: 0, shiftr: 7 }));
    assert_eq!(operations(0, 2).unwrap(), vec!(Op { mask: 0b11000000, shiftl: 0, shiftr: 6 }));
    assert_eq!(operations(0, 3).unwrap(), vec!(Op { mask: 0b11100000, shiftl: 0, shiftr: 5 }));
    assert_eq!(operations(0, 4).unwrap(), vec!(Op { mask: 0b11110000, shiftl: 0, shiftr: 4 }));
    assert_eq!(operations(0, 5).unwrap(), vec!(Op { mask: 0b11111000, shiftl: 0, shiftr: 3 }));
    assert_eq!(operations(0, 6).unwrap(), vec!(Op { mask: 0b11111100, shiftl: 0, shiftr: 2 }));
    assert_eq!(operations(0, 7).unwrap(), vec!(Op { mask: 0b11111110, shiftl: 0, shiftr: 1 }));
    assert_eq!(operations(0, 8).unwrap(), vec!(Op { mask: 0b11111111, shiftl: 0, shiftr: 0 }));
    assert_eq!(operations(0, 9).unwrap(), vec!(Op { mask: 0b11111111, shiftl: 1, shiftr: 0 },
                                               Op { mask: 0b10000000, shiftl: 0, shiftr: 7 }));
    assert_eq!(operations(0, 10).unwrap(), vec!(Op { mask: 0b11111111, shiftl: 2, shiftr: 0 },
                                                Op { mask: 0b11000000, shiftl: 0, shiftr: 6 }));

    assert_eq!(operations(1, 1).unwrap(), vec!(Op { mask: 0b01000000, shiftl: 0, shiftr: 6 }));
    assert_eq!(operations(1, 2).unwrap(), vec!(Op { mask: 0b01100000, shiftl: 0, shiftr: 5 }));
    assert_eq!(operations(1, 3).unwrap(), vec!(Op { mask: 0b01110000, shiftl: 0, shiftr: 4 }));
    assert_eq!(operations(1, 4).unwrap(), vec!(Op { mask: 0b01111000, shiftl: 0, shiftr: 3 }));
    assert_eq!(operations(1, 5).unwrap(), vec!(Op { mask: 0b01111100, shiftl: 0, shiftr: 2 }));
    assert_eq!(operations(1, 6).unwrap(), vec!(Op { mask: 0b01111110, shiftl: 0, shiftr: 1 }));
    assert_eq!(operations(1, 7).unwrap(), vec!(Op { mask: 0b01111111, shiftl: 0, shiftr: 0 }));
    assert_eq!(operations(1, 8).unwrap(), vec!(Op { mask: 0b01111111, shiftl: 1, shiftr: 0 },
                                               Op { mask: 0b10000000, shiftl: 0, shiftr: 7 }));
    assert_eq!(operations(1, 9).unwrap(), vec!(Op { mask: 0b01111111, shiftl: 2, shiftr: 0 },
                                               Op { mask: 0b11000000, shiftl: 0, shiftr: 6 }));

    assert_eq!(operations(8, 1), None);
    assert_eq!(operations(3, 0), None);
    assert_eq!(operations(3, 65), None);

    assert_eq!(operations(3, 33).unwrap(), vec!(Op { mask: 0b00011111, shiftl: 28, shiftr: 0 },
                                                Op { mask: 0b11111111, shiftl: 20, shiftr: 0 },
                                                Op { mask: 0b11111111, shiftl: 12, shiftr: 0 },
                                                Op { mask: 0b11111111, shiftl: 4, shiftr: 0 },
                                                Op { mask: 0b11110000, shiftl: 0, shiftr: 4 }));
}

/// Takes a set of operations to get a field in big endian, and converts them to get the field in
/// little endian.
fn to_little_endian(ops: Vec<Operation>) -> Vec<Operation> {
    unimplemented!()
}

/// Converts a set of operations which would get a field, to a set of operations which would set
/// the field
fn to_mutator(ops: &[Operation]) -> Vec<Operation> {
    unimplemented!()
}

#[plugin_registrar]
pub fn plugin_registrar(registry: &mut Registry) {
    registry.register_syntax_extension(token::intern("packet"),
                                       Decorator(Box::new(generate_packet)));
}

fn generate_packet(ecx: &mut ExtCtxt,
                   span: Span,
                   _meta_item: &ast::MetaItem,
                   item: &ast::Item,
                   mut push: &mut FnMut(P<ast::Item>)) {
    match item.node {
        ast::ItemEnum(..) => unimplemented!(),
        ast::ItemStruct(ref sd, ref _gs) => {
            let name = item.ident.as_str().to_string();
            let header = format!("{}Header", name);
            let mut_header = format!("Mutable{}Header", name);
            push(generate_header_struct(ecx, &header[..], false));
            push(generate_header_struct(ecx, &mut_header[..], true));
            let header_impls = generate_header_impls(ecx, span, &header[..], sd, false);
            if let Some(hi) = header_impls {
                push(hi);
            }
            let header_impls = generate_header_impls(ecx, span, &mut_header[..], sd, true);
            if let Some(hi) = header_impls {
                push(hi);
            }
        },
        _ => {
            ecx.span_err(span, "#[packet] may only be used with enums and structs");
        }
    }
}

fn generate_header_struct(ecx: &mut ExtCtxt, name: &str, mut_: bool) -> P<ast::Item> {
    let mutable = if mut_ {
        " mut"
    } else {
        ""
    };

    ecx.parse_item(format!("//#[derive(Copy)] // FIXME?
struct {}<'p> {{
    packet: &'p{} [u8],
}}", name, mutable))
}

/// Given a type in the form `u([0-9]+)(_be|_le)?`, return a tuple of it's size and endianness
///
/// If 1 <= size <= 8, Endianness will be Big.
fn parse_ty(ty: &str) -> Option<(usize, Endianness)> {
    let re = Regex::new(r"^u([0-9]+)(_be|_le)?$").unwrap();
    let iter = match re.captures_iter(ty).next() {
        Some(c) => c,
        None => return None,
    };

    if iter.len() == 3 || iter.len() == 2 {
        let size = iter.at(1).unwrap();
        let endianness = if let Some(e) = iter.at(2) {
            if e == "_be" {
                Endianness::Big
            } else {
                Endianness::Little
            }
        } else {
            Endianness::Big
        };

        if let Ok(sz) = size.parse() {
            Some((sz, endianness))
        } else {
            None
        }
    } else {
        None
    }
}

#[test]
fn test_parse_ty() {
    assert_eq!(parse_ty("u8"), Some((8, Endianness::Big)));
    assert_eq!(parse_ty("u21_be"), Some((21, Endianness::Big)));
    assert_eq!(parse_ty("u21_le"), Some((21, Endianness::Little)));
    assert_eq!(parse_ty("uab_le"), None);
    assert_eq!(parse_ty("u21_re"), None);
    assert_eq!(parse_ty("i21_be"), None);
}

/// Given the name of a field, and a set of operations required to set that field, return
/// the Rust code required to set the field
fn generate_mutator_str(name: &str, ty: &str, operations: &[Operation]) -> String {
    unimplemented!()
}

/// Given the name of a field, and a set of operations required to get the value of that field,
/// return the Rust code required to get the field.
fn generate_accessor_str(name: &str, ty: &str, offset: usize, operations: &[Operation]) -> String {
    fn build_return(max: usize) -> String {
        let mut ret = "".to_string();
        for i in range(0, max) {
            ret = ret + format!("b{} | ", i).as_slice();
        }
        let new_len = ret.len() - 3;
        ret.truncate(new_len);

        ret
    }

    let op_strings = if operations.len() == 1 {
        let replacement_str = format!("self.packet()[{}]", offset);
        operations.first().unwrap().to_string().replace("{}", &replacement_str[..])
    } else {
        let mut op_strings = "".to_string();
        for (idx, operation) in operations.iter().enumerate() {
            let replacement_str = format!("self.packet()[{}]", offset + idx);
            let operation = operation.to_string().replace("{}", &replacement_str[..]);
            op_strings = op_strings + format!("let b{} = {};\n", idx, operation).as_slice();
        }
        op_strings = op_strings + format!("\n{}\n", build_return(operations.len())).as_slice();

        op_strings
    };

    let accessor = format!("fn get_{name}(&self) -> {ty} {{
            {operations}
            }}", name = name, ty = ty, operations = op_strings);

    accessor
}

fn generate_header_impls(ecx: &mut ExtCtxt, span: Span, name: &str,
                         sd: &ast::StructDef, mut_: bool) -> Option<P<ast::Item>> {

    // FIXME generate getters/setters
    // TODO impl Packet for ...
    let mut bit_offset = 0;
    let ref fields = sd.fields;
    let mut accessors = "".to_string();
    let mut mutators = "".to_string();
    let mut error = false;
    for ref field in fields {
        if let Some(name) = field.node.ident() {
            if let ast::Ty_::TyPath(_, ref path) = field.node.ty.node {
                let ty_str = path.segments.iter().last().unwrap().identifier.as_str();
                println!("field: {:?}", name.as_str());
                println!("field: {:?}", ty_str);
                if let Some((size, endianness)) = parse_ty(ty_str) {
                    // FIXME Don't unwrap
                    let mut ops = operations(bit_offset, size).unwrap();
                    if endianness == Endianness::Little {
                        ops = to_little_endian(ops);
                    }
                    if mut_ {
                        generate_mutator_str(name.as_str(), ty_str, to_mutator(&ops[..]).as_slice());
                    }
                    println!("get: {}", generate_accessor_str(name.as_str(), ty_str, bit_offset / 8, ops.as_slice()));
                } else {
                    // TODO ERROR
                }
            } else {
                ecx.span_err(field.span, "unsupported field type");
                error = true;
            }
        } else {
            ecx.span_err(field.span, "all fields in a packet must be named");
            error = true;
        }
    }

    if error {
        return None;
    }

    let imp = ecx.parse_item(format!("impl<'a> {name}<'a> {{
    pub fn new<'p>(packet: &'p {mut} [u8]) -> {name}<'p> {{
        {name} {{ packet: packet }}
    }}

    {accessors}

    {mutators}
}}", name = name,
     mut = if mut_ { "mut"} else { "" },
     accessors = accessors,
     mutators = mutators
     ));

    Some(imp)
}
