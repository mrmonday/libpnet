// Copyright (c) 2015 Robert Clipsham <robert@octarineparrot.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Linting and other functionality which requires type information

use syntax::ast;
use syntax::parse::token;
use rustc::lint::{Context, LintPass, LintPassObject, LintArray};
use rustc::middle::ty::{node_id_to_type_opt, ty_to_def_id};
use rustc::plugin::Registry;

declare_lint! {
    PACKET_LINT,
    Forbid,
    "additional type checking for #[packet] structs and enums"
}

pub struct PacketPass;

impl LintPass for PacketPass {
    fn get_lints(&self) -> LintArray {
        lint_array!(PACKET_LINT)
    }
    fn check_item(&mut self, ctxt: &Context, item: &ast::Item) {
        //println!("item: {}", item.ident.as_str().to_string());
        if let Some(nid) = node_id_to_type_opt(ctxt.tcx, item.id) {
            //println!("has nid");
            if let Some(def_id) = ty_to_def_id(nid) {
            //println!("has did");
            //    println!("impls {:?}", ctxt.tcx.trait_impls.borrow().get(&def_id));
            } else {
            //println!("no did");
            }
        } else {
            //println!("no nid");

        }
        //println!("traits: {:?}", ctxt.tcx.trait_defs);
        //println!("attrs: {:?}", item.attrs);
        // Find any structs/enums marked as #[packet],
        // for each member field, ensure that either:
        //  * it's a u<N>_<be/le>, or
        //  * it implements some trait to give the on the wire repr/reverse it
        //  * it's a type made of up the above
        // make sure there's some way for the decorator pass to know the size/how
        // to convert it
        //
        // Ensure that there's a #[payload] field which is a slice
    }
}
