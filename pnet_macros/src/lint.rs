// Copyright (c) 2015 Robert Clipsham <robert@octarineparrot.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Linting and other functionality which requires type information

use syntax::ast;
use syntax::attr::AttrMetaMethods;
use syntax::parse::token;
use rustc::lint::{Context, LintPass, LintPassObject, LintArray};
use rustc::middle::ty::{node_id_to_type_opt, ty_to_def_id};
//use rustc::plugin::Registry;

declare_lint! {
    PACKET_LINT,
    Forbid,
    "additional type checking for #[packet] structs and enums"
}

pub struct PacketPass;

fn get_attr<'a>(attrs: &'a [ast::Attribute], name: &str) -> &'a ast::MetaItem_ {
    &attrs.iter().filter(|a| a.check_name(name)).nth(0).unwrap().node.value.node
}

fn has_attr(attrs: &[ast::Attribute], name: &str) -> bool {
    attrs.iter().filter(|a| a.check_name(name)).count() != 0
}

fn check_struct(ctxt: &Context, sd: &ast::StructDef) {
    //let fields = &sd.fields;
    //for ref field in fields {
    //    if has_attr(&field.node.attrs[..], "length_fn") {
    //        match get_attr(&field.node.attrs[..], "length_fn") {
    //            &ast::MetaNameValue(ref s, ref lit) => {
    //                let ref node = lit.node;
    //                match node {
    //                    &ast::LitStr(ref s, _) => {
    //                        //s.to_string()
    //                    },
    //                    _ => panic!("this should be caught before linting")
    //                }
    //            },
    //            _ => panic!("this should be caught before linting")
    //        }
    //    }
    //    //println!("field: {}", field.node.ident().unwrap());
    //    //println!("field ty: {:?}", field.node.ty.node);
    //}
}

impl LintPass for PacketPass {
    fn get_lints(&self) -> LintArray {
        lint_array!(PACKET_LINT)
    }
    fn check_item(&mut self, ctxt: &Context, item: &ast::Item) {
        // Find any structs/enums marked as #[packet],
        // for each member field, ensure that either:
        //  * it's a u<N>_<be/le>, or
        //  * it implements some trait to give the on the wire repr/reverse it
        //  * it's a type made of up the above
        // make sure there's some way for the decorator pass to know the size/how
        // to convert it

        if item.attrs.iter().filter(|a| a.check_name("_packet_lint")).count() == 0 {
            return;
        }
        //println!("item: {}", item.ident.as_str().to_string());
        match item.node {
            ast::ItemEnum(..) => unimplemented!(),
            ast::ItemStruct(ref sd, ref _gs) => {
                check_struct(ctxt, sd);
            },
            _ => panic!("this should be caught before linting")
        }
//println!("      _               _    _               _ _                     
//  ___| |__   ___  ___| | _(_)_ __   __ _  (_) |_ ___ _ __ ___  ___ 
// / __| '_ \\ / _ \\/ __| |/ / | '_ \\ / _` | | | __/ _ \\ '_ ` _ \\/ __|
//| (__| | | |  __/ (__|   <| | | | | (_| | | | ||  __/ | | | | \\__ \\
// \\___|_| |_|\\___|\\___|_|\\_\\_|_| |_|\\__, | |_|\\__\\___|_| |_| |_|___/
//                                   |___/                           
//");
//        println!("item: {}", item.ident.as_str().to_string());
//        println!("attrs: {:?}", item.attrs);

 //       if let Some(nid) = node_id_to_type_opt(ctxt.tcx, item.id) {
 //           println!("has nid");
 //           if let Some(def_id) = ty_to_def_id(nid) {
 //           println!("has did");
 //               println!("impls {:?}", ctxt.tcx.trait_impls.borrow().get(&def_id));
 //           } else {
 //           println!("no did");
 //           }
 //       } else {
 //           println!("no nid");

 //       }
        //println!("traits: {:?}", ctxt.tcx.trait_defs);
        //println!("attrs: {:?}", item.attrs);

    }
}
