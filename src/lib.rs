//
// lib.rs
// Copyright (C) 2015 Adrian Perez <aperez@igalia.com>
// Distributed under terms of the MIT license.
//

pub use self::value::Value;
pub mod value;

pub use self::parser::Parser;
pub mod parser;

pub use self::writer::Writer;
pub mod writer;

use std::collections::BTreeMap;
use std::io::{Cursor, Write, Read};
use std::io;


pub fn parse_string(input: &str) -> parser::Result<Value> {
    Parser::new(Cursor::new(input.as_bytes()).bytes()).parse_message()
}

pub fn parse<Iter>(input: Iter) -> parser::Result<Value>
    where Iter: Iterator<Item=io::Result<u8>>
{
    Parser::new(input).parse_message()
}

pub fn write_string(value: &BTreeMap<String, Value>) -> String {
    let mut output = Cursor::new(Vec::new());
    Writer::pretty(&mut output).write_message(value).ok();
    String::from_utf8(output.into_inner()).unwrap()
}

pub fn write<W>(writer: &mut  W, value: &BTreeMap<String, Value>) -> io::Result<()>
    where W: Write
{
    Writer::pretty(writer).write_message(value)
}
