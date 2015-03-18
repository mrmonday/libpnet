// Copyright (c) 2015 Robert Clipsham <robert@octarineparrot.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//#![warn(missing_docs)]

#![feature(core, plugin_registrar, quote, rustc_private)]

extern crate syntax;
extern crate regex;
#[macro_use] extern crate rustc;

use rustc::lint::{LintPassObject};
use rustc::plugin::Registry;

use syntax::ast;
use syntax::attr;
use syntax::codemap::{Span};
use syntax::parse::token;
use syntax::ext::base::{Decorator, ExtCtxt, Modifier};
use syntax::ptr::P;

mod decorator;
mod lint;
mod util;

/// Replace the #[packet] attribute with internal attributes
///
/// The #[packet] attribute is consumed, so we replace it with two internal attributes,
/// #[_packet_generator], which is used to generate the packet implementations, and
/// #[_packet_lint], which is used to trigger linting.
pub fn packet_modifier(ecx: &mut ExtCtxt,
                   _span: Span,
                   _meta_item: &ast::MetaItem,
                   item: P<ast::Item>) -> P<ast::Item> {
    let mut new_item = (*item).clone();

    new_item.attrs.push(quote_attr!(ecx, #[_packet_lint]));
    new_item.attrs.push(quote_attr!(ecx, #[_packet_generator]));

    P(new_item)
}

#[plugin_registrar]
pub fn plugin_registrar(registry: &mut Registry) {
    registry.register_syntax_extension(token::intern("packet"),
                                       Modifier(Box::new(packet_modifier)));
    registry.register_syntax_extension(token::intern("_packet_generator"),
                                       Decorator(Box::new(decorator::generate_packet)));

    registry.register_lint_pass(Box::new(lint::PacketPass) as LintPassObject);
}
