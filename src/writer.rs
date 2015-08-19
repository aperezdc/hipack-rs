//
// writer.rs
// Copyright (C) 2015 Adrian Perez <aperez@igalia.com>
// Distributed under terms of the MIT license.
//

use std::io;
use std::collections::BTreeMap;
use super::value::Value;


trait Formatter {
    fn start_compound<W>(&mut self, writer: &mut W, ch: u8) -> io::Result<()>
        where W: io::Write;
    fn end_compound<W>(&mut self, writer: &mut W, ch: u8) -> io::Result<()>
        where W: io::Write;
    fn key_separator<W>(&mut self, writer: &mut W, next: &Value) -> io::Result<()>
        where W: io::Write;
    fn item_separator<W>(&mut self, writer: &mut W, next: &Value) -> io::Result<()>
        where W: io::Write;
}


pub struct CompactFormatter;

impl Formatter for CompactFormatter {
    fn start_compound<W>(&mut self, writer: &mut W, ch: u8) -> io::Result<()>
        where W: io::Write
    {
        writer.write_all(&[ch])
    }

    fn end_compound<W>(&mut self, writer: &mut W, ch: u8) -> io::Result<()>
        where W: io::Write
    {
        writer.write_all(&[ch])
    }

    fn key_separator<W>(&mut self, writer: &mut W, next: &Value) -> io::Result<()>
        where W: io::Write
    {
        match *next {
            Value::Dict(_) | Value::List(_) => Ok(()),
            _ => writer.write_all(b":"),
        }
    }

    fn item_separator<W>(&mut self, writer: &mut W, next: &Value) -> io::Result<()>
        where W: io::Write
    {
        match *next {
            Value::Dict(_) | Value::List(_) => Ok(()),
            _ => writer.write_all(b","),
        }
    }
}


pub struct PrettyFormatter {
    indent: usize,
}

impl PrettyFormatter {
    fn new() -> Self {
        PrettyFormatter { indent: 0 }
    }
}

#[inline]
fn indent<W>(writer: &mut W, indent: usize) -> io::Result<()>
    where W: io::Write
{
    for _ in 0..indent {
        try!(writer.write_all(b"  "));
    }
    Ok(())
}

impl Formatter for PrettyFormatter {
    fn start_compound<W>(&mut self, writer: &mut W, ch: u8) -> io::Result<()>
        where W: io::Write
    {
        self.indent += 1;
        try!(writer.write_all(&[ch, b'\n']));
        indent(writer, self.indent)
    }

    fn end_compound<W>(&mut self, writer: &mut W, ch: u8) -> io::Result<()>
        where W: io::Write
    {
        self.indent -= 1;
        try!(writer.write(b"\n"));
        try!(indent(writer, self.indent));
        writer.write_all(&[ch])
    }

    fn key_separator<W>(&mut self, writer: &mut W, next: &Value) -> io::Result<()>
        where W: io::Write
    {
        match *next {
            Value::Dict(_) | Value::List(_) => writer.write_all(b" "),
            _ => writer.write_all(b": "),
        }
    }

    fn item_separator<W>(&mut self, writer: &mut W, next: &Value) -> io::Result<()>
        where W: io::Write
    {
        try!(writer.write(b"\n"));
        indent(writer, self.indent)
    }
}


pub struct Writer<W, F=PrettyFormatter> {
    writer: W,
    format: F,
}


impl<W> Writer<W, CompactFormatter>
    where W: io::Write
{
    #[inline]
    pub fn compact(writer: W) -> Self {
        Writer::with_formatter(writer, CompactFormatter)
    }
}

impl<W> Writer<W>
    where W: io::Write
{
    #[inline]
    pub fn pretty(writer: W) -> Self {
        Writer::with_formatter(writer, PrettyFormatter::new())
    }
}

impl<W, F> Writer<W, F>
    where W: io::Write,
          F: Formatter,
{
    #[inline]
    fn with_formatter(writer: W, formatter: F) -> Self
        where W: io::Write,
              F: Formatter,
    {
        Writer {
            writer: writer,
            format: formatter,
        }
    }

    fn write_value(&mut self, value: &Value) -> io::Result<()> {
        match *value {
            Value::Bool(value) => self.write_bool(value),
            Value::Float(value) => self.write_float(value),
            Value::Integer(value) => self.write_integer(value),
            Value::String(ref value) => self.write_string(value),
            Value::List(ref value) => self.write_list(value),
            Value::Dict(ref value) => self.write_dict(value),
        }
    }

    #[inline]
    fn write_bool(&mut self, value: bool) -> io::Result<()> {
        if value {
            self.writer.write_all(b"True")
        } else {
            self.writer.write_all(b"False")
        }
    }

    #[inline]
    fn write_float(&mut self, value: f64) -> io::Result<()> {
        if value.is_nan() || value.is_infinite() {
            write!(self.writer, "{}", value)
        } else {
            let s = format!("{}", value);
            try!(self.writer.write_all(s.as_bytes()));
            if !s.contains(".") {
                try!(self.writer.write_all(b".0"));
            }
            Ok(())
        }
    }

    #[inline]
    fn write_integer(&mut self, value: i64) -> io::Result<()> {
        write!(self.writer, "{}", value)
    }

    #[inline]
    fn write_string(&mut self, value: &String) -> io::Result<()> {
        try!(self.writer.write_all(b"\""));
        for ch in value.bytes() {
            try!(match ch {
                0x09 => self.writer.write_all(b"\\t"),
                0x0A => self.writer.write_all(b"\\n"),
                0x0D => self.writer.write_all(b"\\r"),
                0x22 => self.writer.write_all(b"\\\""),
                0x5C => self.writer.write_all(b"\\\\"),
                ch if ch < 0xF => write!(self.writer, "\\0{:X}", ch),
                ch if ch < 0x20 => write!(self.writer, "\\{:X}", ch),
                ch => self.writer.write_all(&[ch]),
            });
        }
        self.writer.write_all(b"\"")
    }

    fn write_list(&mut self, value: &Vec<Value>) -> io::Result<()> {
        if value.is_empty() {
            return self.writer.write_all(b"[]");
        }

        let mut iter = value.iter().peekable();
        try!(self.format.start_compound(&mut self.writer, b'['));
        loop {
            match iter.next() {
                None => break,
                Some(value) => {
                    try!(self.write_value(value));
                    match iter.peek () {
                        Some(_) =>
                            try!(self.format.item_separator(&mut self.writer, value)),
                        None => (),
                    }
                },
            }
        }
        self.format.end_compound(&mut self.writer, b']')
    }

    #[inline]
    fn write_dict(&mut self, dict: &BTreeMap<String, Value>) -> io::Result<()> {
        if dict.is_empty() {
            return self.writer.write_all(b"{}");
        }

        try!(self.format.start_compound(&mut self.writer, b'{'));
        try!(self.write_keyval_items(dict));
        self.format.end_compound(&mut self.writer, b'}')
    }

    fn write_keyval_items(&mut self, dict: &BTreeMap<String, Value>) -> io::Result<()> {
        let mut iter = dict.iter().peekable();
        loop {
            match iter.next() {
                None => break,
                Some((key, value)) => {
                    try!(self.writer.write_all(key.as_bytes()));
                    try!(self.format.key_separator(&mut self.writer, value));
                    try!(self.write_value(value));
                    match iter.peek () {
                        Some(&(_, ref value)) =>
                            try!(self.format.item_separator(&mut self.writer, value)),
                        None => (),
                    }
                },
            }
        }
        Ok(())
    }

    #[inline]
    pub fn write_message(&mut self, value: &BTreeMap<String, Value>) -> io::Result<()> {
        self.write_keyval_items(value)
    }

    #[inline]
    pub fn write_framed_message(&mut self, value: &BTreeMap<String, Value>) -> io::Result<()> {
        self.write_dict(value)
    }
}


#[cfg(test)]
mod tests {
	use super::*;
    use super::super::value::Value;
    use std::io::Cursor;
    use std::collections::BTreeMap;
    use std::f64::{NAN, INFINITY};

    macro_rules! make_write_test {
        ($name:ident, $value:expr, $pretty:expr, $compact:expr) => {
            #[test]
            fn $name() {
                {
                    println!("Pretty");
                    let mut output = Cursor::new(Vec::new());
                    Writer::pretty(&mut output).write_value($value).ok();
                    assert_eq!($pretty, String::from_utf8(output.into_inner()).unwrap());
                }
                {
                    println!("Compact");
                    let mut output = Cursor::new(Vec::new());
                    Writer::compact(&mut output).write_value($value).ok();
                    assert_eq!($compact, String::from_utf8(output.into_inner()).unwrap());
                }
            }
        }
    }

    make_write_test!(bool_true,  &Value::Bool(true),  "True", "True");
    make_write_test!(bool_false, &Value::Bool(false), "False", "False");

    make_write_test!(list_empty, &Value::List(vec![]), "[]", "[]");
    make_write_test!(list_one,   &Value::List(vec![Value::Bool(true)]), "[\n  True\n]", "[True]");
    make_write_test!(list_two,   &Value::List(vec![Value::Bool(true), Value::Bool(false)]),
        "[\n  True\n  False\n]", "[True,False]");
    make_write_test!(list_nested,
                     &Value::List(vec![
                                  Value::Bool(true),
                                  Value::List(vec![Value::Bool(false)])]),
                     "[\n  True\n  [\n    False\n  ]\n]", "[True,[False]]");

    make_write_test!(dict_empty, &Value::Dict(BTreeMap::new()), "{}", "{}");
    make_write_test!(dict_one,   &Value::Dict((|| {
        let mut b = BTreeMap::new();
        b.insert("item".to_string(), Value::Bool(true));
        b
    })()), "{\n  item: True\n}", "{item:True}");
    make_write_test!(dict_two,   &Value::Dict((|| {
        let mut b = BTreeMap::new();
        b.insert("foo".to_string(), Value::Bool(true));
        b.insert("bar".to_string(), Value::Bool(false));
        b
    })()), "{\n  bar: False\n  foo: True\n}", "{bar:False,foo:True}");

    macro_rules! make_write_string_tests {
        ($($name:ident, $value:expr, $expected:expr),+) => {
            $( make_write_test!($name, &Value::String($value.to_string()), $expected, $expected); )*
        }
    }

    make_write_string_tests!(string_empty, "", "\"\"",
                             string_non_empty, "foo bar", "\"foo bar\"",
                             string_unicode, "☺", "\"☺\"",
                             string_escapes, "\n\r\t\\\"", "\"\\n\\r\\t\\\\\\\"\"",
                             string_hexcode, "\0", "\"\\00\"");


    macro_rules! make_write_number_tests {
        ($t:ident, $($name:ident, $value:expr, $expected:expr),+) => {
            $( make_write_test!($name, &Value::$t($value), $expected, $expected); )*
        }
    }

    make_write_number_tests!(Integer,
                             integer_zero, 0, "0",
                             integer_negative, -34, "-34");
    make_write_number_tests!(Float,
                             float_zero, 0.0, "0.0",
                             float_suffix, 1f64, "1.0",
                             float_positive, 4.5, "4.5",
                             float_negative, -3.2, "-3.2",
                             float_nan, NAN, "NaN",
                             float_infinite, INFINITY, "inf");
}
