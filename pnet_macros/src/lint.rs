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
use rustc::plugin::Registry;

declare_lint!(PACKET_LINT, Forbid, "Additional type checking for #[packet] structs and enums");

pub struct PacketPass;

impl LintPass for Pass {
    fn get_lints(&self) -> LintArray {
        lint_array!(PACKET_LINT)
    }
    fn check_item(&mut self, ctxt: &Context, item: &Item) {
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
