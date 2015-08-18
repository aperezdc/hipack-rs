//
// main.rs
// Copyright (C) 2015 Adrian Perez <aperez@igalia.com>
// Distributed under terms of the MIT license.
//

extern crate hipack;
use std::io::{Read, stdin, stdout};

fn main() {
    let v = hipack::parse(stdin().bytes()).unwrap();
    hipack::write(&mut stdout(), v.as_dict().unwrap()).ok();
}
