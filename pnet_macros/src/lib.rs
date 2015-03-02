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

use rustc::lint::{LintPassObject};
use rustc::plugin::Registry;

use syntax::parse::token;
use syntax::ext::base::{Decorator};

mod decorator;
mod lint;
mod util;

#[plugin_registrar]
pub fn plugin_registrar(registry: &mut Registry) {
    registry.register_syntax_extension(token::intern("packet"),
                                       Decorator(Box::new(decorator::generate_packet)));
    register.register_lint_pass(Box::new(PacketPass as LintPassObject));
}
