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
#[macro_use] extern crate rustc;

use rustc::lint::{LintPassObject};
use rustc::plugin::Registry;

use syntax::ast;
use syntax::codemap::{Span};
use syntax::parse::token;
use syntax::ext::base::{Decorator, ExtCtxt, Modifier};
use syntax::ptr::P;

mod decorator;
mod lint;
mod util;

pub fn dummy_decorator(_ecx: &mut ExtCtxt,
                   _span: Span,
                   _meta_item: &ast::MetaItem,
                   _item: &ast::Item,
                   mut _push: &mut FnMut(P<ast::Item>)) {
    //_ecx.span_err(_span, "grumble");
}

pub fn dummy_modifier(_ecx: &mut ExtCtxt,
                   _span: Span,
                   _meta_item: &ast::MetaItem,
                   item: P<ast::Item>) -> P<ast::Item> {
    item.clone()
}

#[plugin_registrar]
pub fn plugin_registrar(registry: &mut Registry) {
    //registry.register_syntax_extension(token::intern("payload"),
    //                                   Decorator(Box::new(dummy_decorator)));
    registry.register_syntax_extension(token::intern("packet"),
                                       Decorator(Box::new(decorator::generate_packet)));

    //registry.register_lint_pass(Box::new(lint::PacketPass) as LintPassObject);
}
