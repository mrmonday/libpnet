// Copyright (c) 2015 Robert Clipsham <robert@octarineparrot.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Implements the #[packet] decorator

use regex::Regex;

use std::io;

use syntax::ast;
use syntax::codemap;
use syntax::codemap::{Span};
use syntax::diagnostic;
use syntax::ext::base::{ExtCtxt};
use syntax::ext::build::AstBuilder;
use syntax::ext::quote::rt::ExtParseUtils;
use syntax::parse;
use syntax::ptr::P;

use util::{Endianness, GetOperation, SetOperation, to_little_endian, operations, to_mutator};

struct NullWriter;

impl io::Write for NullWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        println!("write got called!");
        Ok(buf.len())
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

struct NullEmitter;

impl diagnostic::Emitter for NullEmitter {
    fn emit(&mut self,
            _cmsp: Option<(&codemap::CodeMap, Span)>,
            _msg: &str,
            _code: Option<&str>,
            _lvl: diagnostic::Level) {
    }
    fn custom_emit(&mut self,
                   _cm: &codemap::CodeMap,
                   _sp: diagnostic::RenderSpan,
                   _msg: &str,
                   _lvl: diagnostic::Level) {
    }
}

fn null_parse_item(ecx: &mut ExtCtxt, s: String) -> Option<P<ast::Item>> {
    //let sh = diagnostic::mk_span_handler(diagnostic::mk_handler(true, Box::new(NullEmitter)),
    let ew = diagnostic::EmitterWriter::new(Box::new(NullWriter), None);
    let sh = diagnostic::mk_span_handler(diagnostic::mk_handler(true, Box::new(ew)),
                                         codemap::CodeMap::new());
    let sess = parse::new_parse_sess_special_handler(sh);

    //println!("ecx cfg: {:?}", ecx.cfg());
    //println!("ecx sess: {:?}", ecx.parse_sess());

    parse::parse_item_from_source_str("<quote expansion>".to_string(), s, ecx.cfg(), &sess)
}

/// Lower and upper bounds of a payload
/// Represented as strings since they may involve functions.
struct PayloadBounds {
    lower: String,
    upper: String,
}

pub fn generate_packet(ecx: &mut ExtCtxt,
                   span: Span,
                   _meta_item: &ast::MetaItem,
                   item: &ast::Item,
                   mut push: &mut FnMut(P<ast::Item>)) {
    match item.node {
        ast::ItemEnum(..) => unimplemented!(),
        ast::ItemStruct(ref sd, ref _gs) => {
            let name = item.ident.as_str().to_string();
            let header = format!("{}Packet", name);
            let mut_header = format!("Mutable{}Packet", name);
            push(generate_header_struct(ecx, &header[..], false));
            push(generate_header_struct(ecx, &mut_header[..], true));

            let mut payload_bounds = None;

            let header_impls = generate_header_impls(ecx, &span, &header[..], &header[..], sd, false, &mut payload_bounds);
            match header_impls {
                Some((packet_len, hi)) => {
                    push(hi);
                    push(generate_packet_size(ecx, &header[..], &packet_len[..]));
                },
                _ => return,
            }

            let header_impls = generate_header_impls(ecx, &span, &mut_header[..], &header[..], sd, true, &mut payload_bounds);
            match header_impls {
                Some((_, hi)) => push(hi),
                _ => return,
            }

            let payload_bounds = payload_bounds.unwrap();

            //push(ecx.parse_item("use pnet::packet::*;".to_string()));

            if let Some(imp) = generate_packet_impl(ecx, &header[..], &payload_bounds, false) {
                //push(ecx.parse_item("use pnet::packet::Packet;".to_string()));
                push(imp);
            } else {
                ecx.span_err(span, "length_fn must be of type &PacketWithPayloadHeader -> usize");
                return;
            }
            if let Some(imp) = generate_packet_impl(ecx, &mut_header[..], &payload_bounds, false) {
                push(imp);
            } else {
                println!("blargh");
                return;
            }
            if let Some(imp) = generate_packet_impl(ecx, &mut_header[..], &payload_bounds, true) {
                //push(ecx.parse_item("use pnet::packet::MutablePacket;".to_string()));
                push(imp);
            } else {
                println!("blargh");
                return;
            }

            for i in generate_iterable(ecx, &name[..]) {
                push(i);
            }

            let converters = generate_converters(ecx, &name[..], &header[..], &mut_header[..], sd);
            //push(ecx.parse_item("use pnet::packet::FromPacket;".to_string()));
            for c in converters {
                push(c);
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
pub struct {}<'p> {{
    packet: &'p{} [u8],
}}", name, mutable))
}

/// Given a type in the form `u([0-9]+)(be|le)?`, return a tuple of it's size and endianness
///
/// If 1 <= size <= 8, Endianness will be Big.
fn parse_ty(ty: &str) -> Option<(usize, Endianness)> {
    let re = Regex::new(r"^u([0-9]+)(be|le)?$").unwrap();
    let iter = match re.captures_iter(ty).next() {
        Some(c) => c,
        None => return None,
    };

    if iter.len() == 3 || iter.len() == 2 {
        let size = iter.at(1).unwrap();
        let endianness = if let Some(e) = iter.at(2) {
            if e == "be" {
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
    assert_eq!(parse_ty("u21be"), Some((21, Endianness::Big)));
    assert_eq!(parse_ty("u21le"), Some((21, Endianness::Little)));
    assert_eq!(parse_ty("uable"), None);
    assert_eq!(parse_ty("u21re"), None);
    assert_eq!(parse_ty("i21be"), None);
}

/// Given the name of a field, and a set of operations required to set that field, return
/// the Rust code required to set the field
fn generate_mutator_str(name: &str, ty: &str, offset: &str, operations: &[SetOperation]) -> String {
    let mut op_strings = "".to_string();
    for (idx, sop) in operations.iter().enumerate() {
        let pkt_replace = format!("self.packet[{} + {}]", offset, idx);
        let val_replace = "val";
        let sop = sop.to_string().replace("{packet}", &pkt_replace[..])
                                 .replace("{val}", val_replace);
        op_strings = op_strings + &sop[..] + ";\n";
    }

    let mutator = format!("#[inline]
pub fn set_{name}(&mut self, val: {ty}) {{
    {operations}
}}", name = name, ty = ty, operations = op_strings);

    mutator
}

/// Given the name of a field, and a set of operations required to get the value of that field,
/// return the Rust code required to get the field.
fn generate_accessor_str(name: &str,
                         ty: &str,
                         offset: &str,
                         operations: &[GetOperation],
                         inner: Option<&str>)
    -> String
{
    fn build_return(max: usize) -> String {
        let mut ret = "".to_string();
        for i in 0..max {
            ret = ret + &format!("b{} | ", i)[..];
        }
        let new_len = ret.len() - 3;
        ret.truncate(new_len);

        ret
    }

    let op_strings = if operations.len() == 1 {
        let replacement_str = format!("self_.packet[{}]", offset);
        operations.first().unwrap().to_string().replace("{}", &replacement_str[..])
    } else {
        let mut op_strings = "".to_string();
        for (idx, operation) in operations.iter().enumerate() {
            let replacement_str = format!("self_.packet[{} + {}]", offset, idx);
            let operation = operation.to_string().replace("{}", &replacement_str[..]);
            op_strings = op_strings + &format!("let b{} = ({}) as {};\n", idx, operation, ty)[..];
        }
        op_strings = op_strings + &format!("\n{}\n", build_return(operations.len()))[..];

        op_strings
    };

    let accessor = if let Some(struct_name) = inner {
        format!("#[inline]
        fn get_{name}(self_: &{struct_name}) -> {ty} {{
            {operations}
        }}", struct_name = struct_name, name = name, ty = ty, operations = op_strings)
    } else {
        format!("#[inline]
        pub fn get_{name}(&self) -> {ty} {{
            let self_ = self;
            {operations}
        }}", name = name, ty = ty, operations = op_strings)
    };

    accessor
}

fn generate_packet_impl(ecx: &mut ExtCtxt, name: &str, payload_bounds: &PayloadBounds, mut_: bool)
    -> Option<P<ast::Item>>
{
    let mut pre = "".to_string();
    let mut start = "".to_string();
    let mut end = "".to_string();
    if payload_bounds.lower.len() > 0 {
        pre = pre + &format!("let start = {};", payload_bounds.lower)[..];
        start = "start".to_string();
    }
    if payload_bounds.upper.len() > 0 {
        pre = pre + &format!("let end = {};", payload_bounds.upper)[..];
        end = "end".to_string();
    }
    let (mutable, u_mut, mut_) = if mut_ {
        ("Mutable", "_mut", "mut")
    } else {
        ("", "", "")
    };
    // FIXME Should .packet() return the entire buffer, or just the calculated size? what about
    //       checksums etc?
    let item = null_parse_item(ecx, format!("impl<'a> ::pnet::packet::{mutable}Packet for {name}<'a> {{
        #[inline]
        fn packet{u_mut}<'p>(&'p {mut_} self) -> &'p {mut_} [u8] {{ &{mut_} self.packet[..] }}

        #[inline]
        fn payload{u_mut}<'p>(&'p {mut_} self) -> &'p {mut_} [u8] {{
            {pre}
            &{mut_} self.packet[{start}..{end}]
        }}
    }}", name = name, start = start, end = end, pre = pre, mutable = mutable, u_mut = u_mut, mut_ = mut_));

    item
}

fn current_offset(bit_offset: usize, offset_fns: &[String]) -> String {
    let base_offset = if bit_offset == 0 {
        0
    } else {
        (bit_offset - 1) / 8
    };

    offset_fns.iter().fold(base_offset.to_string(), |a, b| {
        a + " + " + &b[..] + "(&self.to_immutable())"
    })
}

fn is_payload(field: &ast::StructField) -> bool {
    field.node.attrs.iter().filter(|&item| {
        let ref node = item.node.value.node;
        match node {
            &ast::MetaWord(ref s) => &s[..] == "payload",
            _ => false
        }
    }).count() > 0
}

fn is_iterable(field: &ast::StructField) -> bool {
    let ref ty = field.node.ty.node;

    if let &ast::Ty_::TyRptr(_, ref ty) = ty {
        if let ast::Ty_::TyVec(ref ty) = ty.ty.node {
            if let ast::Ty_::TyPath(_, ref path) = ty.node {
                let ty_str = path.segments.iter().last().unwrap().identifier.as_str();
                if let None = parse_ty(ty_str) {
                    return true;
                }
            }
        }
    }

    false
}

fn generate_get_fields(sd: &ast::StructDef) -> String {
    let mut gets = String::new();
    let fields = &sd.fields;
    for ref field in fields.iter() {
        match field.node.ident() {
            Some(name) => {
                if is_payload(field) {
                    gets = gets + &format!("{field} : self.payload(),\n", field = name)[..]
                } else if is_iterable(field) {
                    gets = gets + &format!("{field} :
                                               if let Some(val) = self.get_{field}().next() {{
                                                   val.packet
                                               }} else {{
                                                   &[]
                                               }},\n", field = name)[..]
                } else {
                    gets = gets + &format!("{field} : self.get_{field}(),\n", field = name)[..]
                }
            },
            None => panic!(),
        }
    }

    gets
}

fn generate_converters(ecx: &mut ExtCtxt, name: &str, imm_packet: &str, mut_packet: &str,
                         sd: &ast::StructDef) -> Vec<P<ast::Item>> {
    let mut items = Vec::with_capacity(4);

    //let packet_len = "1/* TODO */";
    //let set_fields = "/* TODO */";
    let get_fields = generate_get_fields(sd);

    //let item = ecx.parse_item(format!("impl<'p> ToMutablePacket<'p> for {name}<'p> {{
    //    type T = {mut_packet}<'p>;
    //    #[inline]
    //    fn to_packet_mut(&self, buf: &'p mut [u8]) -> {mut_packet}<'p> {{
    //        let mut packet = {mut_packet}::new(buf);

    //        {set_fields}

    //        packet
    //    }}
    //}}", name = name, mut_packet = mut_packet, set_fields = set_fields));
    //items.push(item);

    //let item = ecx.parse_item(format!("impl<'p> ToPacket<'p> for {name}<'p> {{
    //    type T = {imm_packet}<'p>;
    //    #[inline]
    //    fn to_packet(&self, buf: &'p mut [u8]) -> {imm_packet}<'p> {{
    //        self.to_packet_mut(buf).to_immutable()
    //    }}
    //}}", name = name, imm_packet = imm_packet));
    //items.push(item);

    for packet in &[imm_packet, mut_packet] {
        let item = ecx.parse_item(format!("
        impl<'p> ::pnet::packet::FromPacket<'p> for {packet}<'p> {{
            type T = {name}<'p>;
            #[inline]
            fn from_packet(&'p self) -> {name}<'p> {{
                use pnet::packet::Packet;
                {name} {{
                    {get_fields}
                }}
            }}
        }}", packet = packet, name = name, get_fields = get_fields));
        items.push(item);
    }

    items
}

fn generate_packet_size(ecx: &mut ExtCtxt, name: &str, size: &str) -> P<ast::Item> {
    let item = ecx.parse_item(format!("
        impl<'a> ::pnet::packet::PacketSize for {name}<'a> {{
            fn packet_size(&self) -> usize {{
                {size}
            }}
        }}
    ", name = name, size = size));

    item
}

fn generate_iterable(ecx: &mut ExtCtxt, name: &str) -> Vec<P<ast::Item>> {
    let item1 = ecx.parse_item(format!("
    // TODO C-like enum fields?
    // TODO auto-impl a packet size struct?
    pub struct {name}Iterable<'a> {{
        buf: &'a [u8],
    }}
    ", name = name));
    let item2 = ecx.parse_item(format!("
    impl<'a> Iterator for {name}Iterable<'a> {{
        type Item = {name}Packet<'a>;

        fn next(&mut self) -> Option<{name}Packet<'a>> {{
            use pnet::packet::PacketSize;
            if self.buf.len() > 0 {{
                let ret = {name}Packet::new(self.buf);
                self.buf = &self.buf[ret.packet_size()..];

                return Some(ret);
            }}

            None
        }}

        fn size_hint(&self) -> (usize, Option<usize>) {{
            (0, None)
        }}
    }}
    ", name = name));
    println!("iterable: {}Iterable", name);

    vec![item1, item2]
}

fn generate_header_impls(ecx: &mut ExtCtxt, span: &Span, name: &str, imm_name: &str,
                         sd: &ast::StructDef, mut_: bool, payload_fn: &mut Option<PayloadBounds>)
    -> Option<(String, P<ast::Item>)>
{
    let mut bit_offset = 0;
    let mut offset_fns = Vec::new();
    let ref fields = sd.fields;
    let mut accessors = "".to_string();
    let mut mutators = "".to_string();
    let mut error = false;
    let mut found_payload = false;
    let mut payload_span = None;
    for (idx, ref field) in fields.iter().enumerate() {
        //println!("field: {:?}", field);

        if let Some(field_name) = field.node.ident() {
            let co = current_offset(bit_offset, &offset_fns[..]);

            // FIXME No need for has_length_fn
            let (has_length_fn, length_fn) = field.node.attrs.iter()
                                                  .fold((false, None), |(has, name), b| {
                if !has {
                    let ref node = b.node.value.node;
                    match node {
                        &ast::MetaNameValue(ref s, ref lit) => if &s[..] == "length_fn" {
                            let ref node = lit.node;
                            match node {
                                &ast::LitStr(ref s, _) => {
                                    // FIXME Do we need to check the style (snd lit.node)?
                                    (true, Some(s.to_string()))
                                },
                                _ => {
                                    ecx.span_err(field.span, "#[length_fn] should be used as #[length_fn = \"name_of_function\"]");
                                    error = true;
                                    (has, name)
                                }
                            }
                        } else {
                            (has, name)
                        },
                        _ => (has, name)
                    }
                } else {
                    (has, name)
                }
            });

            let construct_with = field.node.attrs.iter().fold(Vec::new(), |mut tys, item| {
                let ref node = item.node.value.node;
                match node {
                    &ast::MetaList(ref s, ref items) => {
                        if &s[..] == "construct_with" {
                            for ty in items.iter() {
                                if let ast::MetaWord(ref s) = ty.node {
                                    tys.push(s.to_string());
                                } else {
                                    ecx.span_err(field.span, "#[construct_with] should be of the form #[construct_with(<types>)]");
                                    error = true;
                                    break;
                                }
                            }
                        }
                    },
                    &ast::MetaWord(ref s) | &ast::MetaNameValue(ref s, _) => {
                        if &s[..] == "construct_with" {
                            ecx.span_err(field.span, "#[construct_with] should be of the form #[construct_with(<types>)]");
                            error = true;
                        }
                    },
                }

                tys
            });

            let is_payload = field.node.attrs.iter().filter(|&item| {
                let ref node = item.node.value.node;
                match node {
                    &ast::MetaWord(ref s) => &s[..] == "payload",
                    &ast::MetaList(ref s, _) |
                    &ast::MetaNameValue(ref s, _) => {
                        if &s[..] == "payload" {
                            ecx.span_err(field.span, "#[payload] attribute has no arguments");
                            error = true;
                        }

                        false
                    },
                }
            }).count() > 0;
            if is_payload {
                if found_payload {
                    ecx.span_err(field.span, "packet may not have multiple payloads");
                    ecx.span_note(payload_span.unwrap(), "first payload defined here");
                    return None;
                }
                found_payload = true;
                payload_span = Some(field.span);
                let mut upper_bound_str = "".to_string();
                if has_length_fn {
                    upper_bound_str = format!("{} + {}(&self.to_immutable())", co.clone(), length_fn.as_ref().unwrap());
                } else {
                    if idx != fields.len() - 1 {
                        ecx.span_err(field.span, "#[payload] must specify a #[length_fn], unless it is the last field of a packet");
                        error = true;
                    }
                }
                *payload_fn = Some(PayloadBounds {
                    lower: co.clone(),
                    upper: upper_bound_str,
                });
            }
            match field.node.ty.node {
                ast::Ty_::TyPath(_, ref path) => {
                    let ty_str = path.segments.iter().last().unwrap().identifier.as_str();
                    if let Some((size, endianness)) = parse_ty(ty_str) {
                        // FIXME Don't unwrap
                        println!("name: {}", name);
                        let mut ops = operations(bit_offset % 8, size).unwrap();
                        if endianness == Endianness::Little {
                            ops = to_little_endian(ops);
                        }
                        if mut_ {
                            mutators = mutators + &generate_mutator_str(field_name.as_str(), ty_str, &co[..], &to_mutator(&ops[..])[..])[..];
                        }
                        accessors = accessors + &generate_accessor_str(field_name.as_str(), ty_str, &co[..], &ops[..], None)[..];
                        bit_offset += size;
                    } else {
                        // NOTE assumes this starts on a byte bountry
                        // If ty_str is not a u*, then either:
                        //  * The type is of fixed size, and is another packet type
                        //  * The type is of variable length, and is an array of another packet type
                        let mut len = 0;
                        let mut inner_accessors = String::new();
                        let mut get_args = String::new();
                        for (i, arg) in construct_with.iter().enumerate() {
                            if let Some((size, endianness)) = parse_ty(arg) {
                                let mut ops = operations(bit_offset % 8, size).unwrap();
                                if endianness == Endianness::Little {
                                    ops = to_little_endian(ops);
                                }
                                let arg_name = format!("arg{}", i);
                                inner_accessors = inner_accessors + &generate_accessor_str(&arg_name[..], arg, &co[..], &ops[..], Some(name))[..];
                                get_args = format!("{}get_{}(&self), ", get_args, arg_name);
                                // TODO Generate mutators, use PrimitiveValues to set.
                                bit_offset += size;
                                len += size;
                            } else {
                                ecx.span_err(field.span, "arguments to #[construct_with] must be primitives");
                                error = true;
                            }
                        }
                        if mut_ {
                            mutators = mutators + &format!("
                            #[inline]
                            fn set_{name}(&mut self, val: {ty_str}) {{
                                let current_offset = {co};

                                //val.to_wire(self.packet[current_offset..current_offset + {len}]);
                            }}
                            ", name = field_name, ty_str = ty_str, co = co, len = len)[..];

                            // FIXME Don't do this if ty_str's not a #[packet]
                            //accessors = accessors + &format!("
                            //    #[inline]
                            //    fn get_{name}_mut(&mut self) -> Mutable{ty_str} {{
                            //        let current_offset = {co};

                            //        Mutable{ty_str}::new(&mut self.packet[current_offset..]);
                            //    }}
                            //    ", name = name, ty_str = ty_str, co = co)[..];
                        }
                        let ctor = if construct_with.len() > 0 {
                            format!("{} {}::new({})", inner_accessors, ty_str, &get_args[..get_args.len() - 2])
                        } else {
                            format!("{}::new(&self.packet[current_offset..])", ty_str)
                        };
                        accessors = accessors + &format!("
                            #[inline]
                            fn get_{name}(&self) -> {ty_str} {{
                                let current_offset = {co};

                                {ctor}
                            }}
                            ", name = field_name, ty_str = ty_str, ctor = ctor, co = co)[..];
                    }
                },
                ast::Ty_::TyRptr(_, ref ty) => {
                    let ref ty = ty.ty.node;
                    if let &ast::Ty_::TyVec(ref ty) = ty {
                        if !has_length_fn && !is_payload {
                            ecx.span_err(field.span, "variable length field must have #[length_fn = \"\"] attribute");
                            error = true;
                        }
                        if !is_payload {
                            if let ast::Ty_::TyPath(_, ref path) = ty.node {
                                println!("name: {}", name);
                                let ty_str = path.segments.iter().last().unwrap().identifier.as_str();
                                if let Some(_) = parse_ty(ty_str) {
                                    accessors = accessors + &format!("
                                    #[inline]
                                    fn get_{name}(&self) -> &[{ty_str}] {{
                                        let current_offset = {co};
                                        let len = {length_fn}(&self.to_immutable());

                                        &self.packet[current_offset..len]
                                    }}
                                    ", name = field_name, ty_str = ty_str, co = co, length_fn = length_fn.as_ref().unwrap())[..];
                                } else {
                                    accessors = accessors + &format!("
                                    #[inline]
                                    fn get_{name}(&self) -> {ty_str}Iterable {{
                                        let current_offset = {co};
                                        let len = {length_fn}(&self.to_immutable());

                                        {ty_str}Iterable {{
                                            buf: &self.packet[current_offset..len]
                                        }}
                                    }}
                                    // FIXME Since this allocates, we probably want to deny &[Foo]
                                    //       and require FooIterable
                                    //#[inline]
                                    //fn get_{name}(&self) -> &[{ty_str}] {{
                                    //    use pnet::packet::FromPacket;

                                    //    let current_offset = {co};
                                    //    let len = {length_fn}(&self.to_immutable());

                                    //    {ty_str}Iterable {{
                                    //        buf: &self.packet[current_offset..len]
                                    //    }}.map( |packet| packet.from_packet() )
                                    //      .collect::<Vec<_>>()
                                    //      .as_slice()
                                    //}}
                                    ", name = field_name, ty_str = ty_str, co = co, length_fn = length_fn.as_ref().unwrap())[..];
                                }
                            }
                            // TODO Generate getters/setters
                            // if u**, get N*<type> fields - needs to be an Iterator to not allocate?
                            // otherwise, assume #[packet], get N * type
                        }
                    } else {
                        panic!("unhandled field type");
                    }
                },
                _ => {
                    panic!("unhandled field type");
                }
            }
            if let ast::Ty_::TyPath(_, ref path) = field.node.ty.node {

            } else {
                println!("field type: {:?}", field.node.ty.node);
                // FIXME Do this in lint
                //ecx.span_err(field.span, "unsupported field type");
                //error = true;
            }
            if has_length_fn {
                offset_fns.push(length_fn.unwrap());
            }
        } else {
            ecx.span_err(field.span, "all fields in a packet must be named");
            error = true;
        }
    }

    if !found_payload && !error {
        ecx.span_err(*span, "#[packet]'s must contain a payload");
        return None;
    }

    if error {
        return None;
    }

    let set_fields = "/* TODO */";

    let populate = if mut_ {
        format!("#[inline]
         pub fn populate(str: {name}) {{
             {set_fields}
         }}", name = name, set_fields = set_fields)
    } else {
        "".to_string()
    };

    let imp = ecx.parse_item(format!("impl<'a> {name}<'a> {{
    #[inline]
    pub fn new<'p>(packet: &'p {mut} [u8]) -> {name}<'p> {{
        // TODO This should ensure the provided buffer is at least a minimum size so we can avoid
        //      bounds checking in accessors/mutators
        {name} {{ packet: packet }}
    }}

    #[inline]
    pub fn to_immutable<'p>(&'p self) -> {imm_name}<'p> {{
        match *self {{
            {name} {{ ref packet }} => {imm_name} {{ packet: packet }}
        }}
    }}

    {populate}

    {accessors}

    {mutators}
}}", name = name,
     imm_name = imm_name,
     mut = if mut_ { "mut"} else { "" },
     accessors = accessors,
     mutators = mutators,
     populate = populate
     ));

    let co = current_offset(bit_offset, &offset_fns[..]);
    Some((co, imp))
}

