// Copyright 2015-2016 hipack-rs developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

extern crate hipack;
use std::io::{Read, stdin, stdout};

fn main() {
    let v = hipack::parse(stdin().bytes()).unwrap();
    hipack::write(&mut stdout(), v.as_dict().unwrap()).ok();
}
