//
// parser.rs
// Copyright (C) 2015 Adrian Perez <aperez@igalia.com>
// Distributed under terms of the MIT license.
//

use std::io;
use std::fmt;
use std::error;
use std::result;
use std::collections::BTreeMap;
use super::value::Value;
use std::f64::{NAN, INFINITY};


#[derive(Clone, PartialEq)]
enum ErrorCode {
    ExpectedDictKey,
    ExpectedSeparator,
    InvalidBoolValue,
    InvalidNumberValue,
    InvalidEscapeSequence,
    InvalidUtf8Sequence,
    UnexpectedEOF,
    UnterminatedMessage,
    UnterminatedString,
    UnterminatedList,
    UnterminatedDict,
}


impl fmt::Debug for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (match *self {
            ErrorCode::ExpectedDictKey       => "Dictionary key expected",
            ErrorCode::ExpectedSeparator     => "Separator expected",
            ErrorCode::InvalidBoolValue      => "Invalid boolean value",
            ErrorCode::InvalidNumberValue    => "Invalid numeric value",
            ErrorCode::InvalidEscapeSequence => "Invalid escape sequence",
            ErrorCode::InvalidUtf8Sequence   => "Invalid UTF-8 sequence",
            ErrorCode::UnexpectedEOF         => "Unexpected end of file",
            ErrorCode::UnterminatedMessage   => "Unterminated message",
            ErrorCode::UnterminatedString    => "Unterminated string",
            ErrorCode::UnterminatedList      => "Unterminated list",
            ErrorCode::UnterminatedDict      => "Unterminated dictionary",
        }).fmt(f)
    }
}


#[derive(Debug)]
enum Error {
    SyntaxError(ErrorCode, usize, usize), // Error, line, column
    IoError(io::Error),
}


impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::SyntaxError(..) => "syntax error",
            Error::IoError(ref error) => error::Error::description(error),
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::IoError(ref error) => Some(error),
            _ => None,
        }
    }
}


impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::SyntaxError(ref code, line, column) =>
                write!(f, "{:?} at line {} column {}", code, line, column),
            Error::IoError(ref error) =>
                fmt::Display::fmt(error, f),
        }
    }
}


impl From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::IoError(error)
    }
}


pub type Result<T> = result::Result<T, Error>;


#[inline]
fn is_whitespace_character(ch: u8) -> bool {
    match ch { 0x09  // Horizontal tab.
             | 0x0A  // New line.
             | 0x0D  // Carriage return.
             | 0x20  // Space.
               => true,
             _ => false,
    }
}


#[inline]
fn is_key_character(ch: u8) -> bool {
    match ch {
        b'[' | b']' | b'{' | b'}' | b':' | b',' => false, // Forbidden in keys by the spec
        _ => !is_whitespace_character(ch),
    }
}


#[inline]
fn is_xdigit_character(ch: u8) -> bool {
    match ch { b'0'...b'9'
             | b'a'...b'f'
             | b'A'...b'F'
               => true,
             _ => false,
    }
}


#[inline]
fn xdigit_to_int(ch: u8) -> u8 {
    match ch {
        b'0' => 0,
        b'1' => 1,
        b'2' => 2,
        b'3' => 3,
        b'4' => 4,
        b'5' => 5,
        b'6' => 6,
        b'7' => 7,
        b'8' => 8,
        b'9' => 9,
        b'a' | b'A' => 0xA,
        b'b' | b'B' => 0xB,
        b'c' | b'C' => 0xC,
        b'd' | b'D' => 0xD,
        b'e' | b'E' => 0xE,
        b'f' | b'F' => 0xF,
        _ => panic!("Invalid input {}", ch),
    }
}


#[inline]
fn is_octal_character(ch: u8) -> bool {
    match ch {
        b'0'...b'7' => true,
        _ => false,
    }
}


pub struct Parser<Iter: Iterator<Item=io::Result<u8>>> {
    input : Iter,
    line  : usize,
    column: usize,
    look  : Option<u8>,
}


macro_rules! try_no_eof {
    ($obj:expr, $e:expr) => {
        (match try!($e) {
            None => return $obj.parse_error(ErrorCode::UnexpectedEOF),
            Some(v) => v,
        })
    }
}

macro_rules! peek_no_eof {
    ($e:expr) => {
        match try!($e.peek()) {
            None => return $e.parse_error(ErrorCode::UnexpectedEOF),
            Some(v) => v,
        }
    }
}

macro_rules! getc_no_eof {
    ($e:expr) => {
        match $e.nextchar_raw() {
            None => return $e.parse_error(ErrorCode::UnexpectedEOF),
            Some(Err(error)) => return Err(Error::IoError(error)),
            Some(Ok(ch)) => ch,
        }
    }
}

macro_rules! parse_digits {
    ($e:expr, $base:expr, $check_digit:expr, $negative:expr) => {{
        if !$check_digit(peek_no_eof!($e)) {
            return $e.parse_error(ErrorCode::InvalidNumberValue);
        }
        let mut result = 0i64;
        loop {
            result = result * $base + match try!($e.peek()) {
                Some(ch) if $check_digit(ch) => xdigit_to_int(ch) as i64,
                Some(_) | None => break,
            };
            $e.advance();
        }
        if $negative {
            result = -result;
        }
        Ok(Value::Integer(result))
    }}
}

macro_rules! match_any {
    ($e:expr, $error:ident, $($item:expr),+) => {
        match peek_no_eof!($e) {
            $(
                $item => $e.advance(),
            )*
            _ => { return $e.parse_error(ErrorCode::$error); },
        }
    }
}

impl<Iter> Parser<Iter>
    where Iter: Iterator<Item=io::Result<u8>>
{
    pub fn new(input: Iter) -> Self {
        Parser {
            input : input,
            line  : 1,
            column: 0,
            look  : None,
        }
    }

    fn nextchar_raw(&mut self) -> Option<io::Result<u8>> {
        match self.input.next() {
            Some(Ok(b'\n')) => {
                self.line += 1;
                self.column = 0;
                Some(Ok(b'\n'))
            }
            Some(Ok(c)) => {
                self.column += 1;
                Some(Ok(c))
            }
            x => x,
        }
    }

    fn nextchar(&mut self) -> Option<io::Result<u8>> {
        match self.nextchar_raw() {
            Some(Ok(b'#')) => loop {
                match self.nextchar_raw() {
                    None => return None,
                    Some(Ok(b'\n')) => return self.nextchar(),
                    Some(Ok(..)) => continue,
                    Some(Err(x)) => return Some(Err(x)),
                }
            },
            x => return x,
        }
    }

    fn peek(&mut self) -> Result<Option<u8>> {
        match self.look {
            Some(ch) => Ok(Some(ch)),
            None => {
                match self.nextchar() {
                    Some(Err(error)) => Err(Error::IoError(error)),
                    Some(Ok(ch)) => {
                        self.look = Some(ch);
                        Ok(self.look)
                    },
                    None => Ok(None),
                }
            }
        }
    }

    fn advance(&mut self) {
        self.look = None;
    }

    fn parse_whitespace(&mut self) -> Result<()> {
        match self.peek() {
            Err(t) => Err(t),
            Ok(None) => Ok(()),
            Ok(Some(ch)) if is_whitespace_character(ch) => {
                self.advance();
                self.parse_whitespace()
            },
            Ok(Some(ch)) => {
                self.look = Some(ch);
                return Ok(())
            },
        }
    }

    fn parse_error<T>(&self, code : ErrorCode) -> Result<T> {
        Err(Error::SyntaxError(code, self.line, self.column))
    }

    fn parse_value(&mut self) -> Result<Value> {
        match peek_no_eof!(self) {
            b'"' => self.parse_string(),
            b'[' => self.parse_list(),
            b'{' => self.parse_dict(),
            b'T' | b't' | b'F' | b'f' => self.parse_bool(),
            _ => self.parse_number(),
        }
    }

    fn match_char(&mut self, expected: u8, code: ErrorCode) -> Result<()> {
        if peek_no_eof!(self) == expected {
            self.advance();
            Ok(())
        } else {
            self.parse_error(code)
        }
    }

    fn parse_string(&mut self) -> Result<Value> {
        try!(self.match_char(b'"', ErrorCode::UnterminatedString));
        let mut value = Vec::new();
        loop {
            match getc_no_eof!(self) {
                b'"' => break,
                b'\\' => match getc_no_eof!(self) {
                    b'"'  => value.push(b'"'),
                    b'\\' => value.push(b'\\'),
                    b'n'  => value.push(b'\n'),
                    b'r'  => value.push(b'\r'),
                    b't'  => value.push(b'\t'),
                    c if is_xdigit_character(c) => {
                        // Hex code.
                        let x = match getc_no_eof!(self) {
                            ch if is_xdigit_character(ch) => ch,
                            _ => return self.parse_error(ErrorCode::InvalidEscapeSequence),
                        };
                        value.push(xdigit_to_int(c) * 16 + xdigit_to_int(x));
                    },
                    _ => return self.parse_error(ErrorCode::InvalidEscapeSequence),
                },
                ch => value.push(ch),
            }
        }

        match String::from_utf8(value) {
            Err(_) => self.parse_error(ErrorCode::InvalidUtf8Sequence),
            Ok(x) => Ok(Value::String(x)),
        }
    }

    fn parse_list(&mut self) -> Result<Value> {
        try!(self.match_char(b'[', ErrorCode::UnterminatedList));
        try!(self.parse_whitespace());

        let mut list = Vec::new();
        while peek_no_eof!(self) != b']' {
            // Parse a single value.
            list.push(try!(self.parse_value()));

            // There must be either comma or whitespace.
            let got_whitespace = is_whitespace_character(peek_no_eof!(self));
            try!(self.parse_whitespace());
            if peek_no_eof!(self) == b',' {
                self.advance();
            } else if !got_whitespace && !is_whitespace_character(peek_no_eof!(self)) {
                break;
            }
            try!(self.parse_whitespace());
        }

        try!(self.match_char(b']', ErrorCode::UnterminatedList));
        Ok(Value::List(list))
    }

    fn parse_key(&mut self) -> Result<String> {
        let mut key = Vec::new();
        while is_key_character(peek_no_eof!(self)) {
            key.push(peek_no_eof!(self));
            self.advance();
        }

        if key.len() == 0 {
            self.parse_error(ErrorCode::ExpectedDictKey)
        } else {
            match String::from_utf8(key) {
                Err(_) => self.parse_error(ErrorCode::InvalidUtf8Sequence),
                Ok(x) => Ok(x),
            }
        }
    }

    fn parse_keyval_items(&mut self, delim: Option<u8>) -> Result<Value> {
        let mut items = BTreeMap::new();
        while try!(self.peek()) != delim {
            let key = try!(self.parse_key());
            let mut got_separator = false;
            if is_whitespace_character(peek_no_eof!(self)) {
                try!(self.parse_whitespace());
                got_separator = true;
            }

            match peek_no_eof!(self) {
                b':' => {
                    self.advance();
                    try!(self.parse_whitespace());
                    got_separator = true;
                },
                b'{' | b'[' => {
                    got_separator = true;
                },
                _ => (),
            }
            if !got_separator {
                return self.parse_error(ErrorCode::ExpectedSeparator);
            }

            items.insert(key, try!(self.parse_value()));

            match try!(self.peek()) {
                Some(b',') => self.advance(),
                Some(ch) if Some(ch) != delim =>
                    if !is_whitespace_character(ch) {
                        break;
                    },
                None if None != delim =>
                    return self.parse_error(ErrorCode::UnexpectedEOF),
                _ => (),
            }

            try!(self.parse_whitespace());
        }
        Ok(Value::Dict(items))
    }

    fn parse_dict(&mut self) -> Result<Value> {
        try!(self.match_char(b'{', ErrorCode::UnterminatedDict));
        try!(self.parse_whitespace());
        let result = try!(self.parse_keyval_items(Some(b'}')));
        try!(self.match_char(b'}', ErrorCode::UnterminatedDict));
        Ok(result)
    }

    fn parse_bool_rest(&mut self, result: bool) -> Result<Value> {
        self.advance();
        let remaining : &[u8] = { if result { b"rue" } else { b"alse" } };
        for byte in remaining {
            if peek_no_eof!(self) != *byte {
                return self.parse_error(ErrorCode::InvalidBoolValue);
            }
            self.advance();
        }
        Ok(Value::Bool(result))
    }

    fn parse_bool(&mut self) -> Result<Value> {
        match peek_no_eof!(self) {
            b'T' | b't' => self.parse_bool_rest(true),
            b'F' | b'f' => self.parse_bool_rest(false),
            _ => self.parse_error(ErrorCode::InvalidBoolValue),
        }
    }

    fn parse_number(&mut self) -> Result<Value> {
        let mut buffer = Vec::new();

        // Optional sign.
        let negative = match peek_no_eof!(self) {
            b'-' => { self.advance(); true },
            b'+' => { self.advance(); false },
            _ => false,
        };

        // Detect octal/hex/nan/inf numbers.
        match peek_no_eof!(self) {
            b'0' => {
                buffer.push(b'0');
                self.advance();
                match try!(self.peek()) {
                    None => (),
                    Some(b'x') | Some(b'X') => {
                        buffer.push(b'x');
                        self.advance();
                        return parse_digits!(self, 16, is_xdigit_character, negative)
                    },
                    Some(ch) if is_octal_character(ch) =>
                        return parse_digits!(self, 8, is_octal_character, negative),
                    Some(_) => (),
                }
            },
            b'N' | b'n' => {
                self.advance();
                match_any!(self, InvalidNumberValue, b'a', b'A');
                match_any!(self, InvalidNumberValue, b'n', b'N');
                return Ok(Value::Float(if negative { -NAN } else { NAN }));
            },
            b'I' | b'i' => {
                self.advance();
                match_any!(self, InvalidNumberValue, b'n', b'N');
                match_any!(self, InvalidNumberValue, b'f', b'F');
                match try!(self.peek()) {
                    Some(b'i') | Some(b'I') => {
                        self.advance();
                        match_any!(self, InvalidNumberValue, b'n', b'N');
                        match_any!(self, InvalidNumberValue, b'i', b'I');
                        match_any!(self, InvalidNumberValue, b't', b'T');
                        match_any!(self, InvalidNumberValue, b'y', b'Y');
                    }
                    _ => (),
                }
                return Ok(Value::Float(if negative { -INFINITY } else { INFINITY }));
            },
            _ => (),
        }

        let mut dot_seen = false;
        let mut exp_seen = false;
        loop {
            match try!(self.peek()) {
                None => break,
                Some(b'e') | Some(b'E') => {
                    if exp_seen {
                        return self.parse_error(ErrorCode::InvalidNumberValue);
                    }
                    exp_seen = true;
                    buffer.push(b'e');
                    self.advance();
                    // Optional exponent sign.
                    match peek_no_eof!(self) {
                        ch @b'-' | ch @b'+' => {
                            buffer.push(ch);
                            self.advance();
                        },
                        _ => (),
                    }
                },
                Some(b'.') => {
                    if dot_seen {
                        return self.parse_error(ErrorCode::InvalidNumberValue);
                    }
                    dot_seen = true;
                    buffer.push(b'.');
                    self.advance();
                },
                Some(ch @b'0'...b'9') => {
                    buffer.push(ch);
                    self.advance();
                },
                Some(_) =>
                    return self.parse_error(ErrorCode::InvalidNumberValue),
            }
        }

        if buffer.len() == 0 {
            return self.parse_error(ErrorCode::InvalidNumberValue);
        }

        let string_value = match String::from_utf8(buffer) {
            Err(_) => return self.parse_error(ErrorCode::InvalidUtf8Sequence),
            Ok(value) => value,
        };

        if dot_seen || exp_seen {
            match string_value.parse::<f64>() {
                Ok(value) => Ok(Value::Float(if negative { -value } else { value })),
                Err(_) => self.parse_error(ErrorCode::InvalidNumberValue),
            }
        } else {
            match string_value.parse::<i64>() {
                Ok(value) => Ok(Value::Integer(if negative { -value } else { value })),
                Err(_) => self.parse_error(ErrorCode::InvalidNumberValue),
            }
        }
    }

    pub fn parse_message(&mut self) -> Result<Value> {
        try!(self.parse_whitespace());
        match try!(self.peek()) {
            None => Ok(Value::Dict(BTreeMap::new())),
            Some(b'{') => {
                self.advance();
                try!(self.parse_whitespace());
                let result = try!(self.parse_keyval_items(Some(b'}')));
                try!(self.match_char(b'}', ErrorCode::UnterminatedMessage));
                Ok(result)
            },
            _ => self.parse_keyval_items(None),
        }
    }
}


#[cfg(test)]
mod tests {
	use super::*;
	use super::super::value::Value;
	use std::io::{Cursor, Read};

	#[test]
	fn peek_input() {
	    let input = Cursor::new("Hello, world".as_bytes());
	    let mut p = Parser::new(input.bytes());
	    assert_eq!(Some(b'H'), p.peek().unwrap());
	}

	#[test]
	fn ignore_comment() {
	    let input = Cursor::new("# Comment\nFoo".as_bytes());
	    let mut p = Parser::new(input.bytes());
	    assert_eq!(Some(b'F'), p.peek().unwrap());
    }

    #[test]
    fn ignore_consecutive_comments() {
        let input = Cursor::new("# Comment 1\n#Comment 2\nBar".as_bytes());
        let mut p = Parser::new(input.bytes());
        assert_eq!(Some(b'B'), p.peek().unwrap());
    }

    #[test]
    fn skip_whitespace() {
        let input = Cursor::new("    Whitespace".as_bytes());
        let mut p = Parser::new(input.bytes());
        p.parse_whitespace().unwrap();
        assert_eq!(Some(b'W'), p.peek().unwrap());
    }

    #[test]
    fn advance_input() {
        let hello = "Hello, world";
        let mut check = hello.bytes();
        let input = Cursor::new(hello.as_bytes());
        let mut p = Parser::new(input.bytes());
        loop {
            match p.peek().unwrap() {
                Some(ch) => assert_eq!(check.next().unwrap(), ch),
                None => break,
            }
            p.advance();
        }
    }

    macro_rules! make_fail_tests {
        ($($name:ident, $str:expr),+) => {
            $(
            #[test] #[should_panic]
            fn $name() {
                let input = Cursor::new($str.as_bytes());
                let mut p = Parser::new(input.bytes());
                p.parse_list().unwrap().as_list();
            }
            )*
        }
    }

    macro_rules! make_bool_tests {
        ($expected:expr, $($name:ident, $str:expr),+) => {
            $(
            #[test]
            fn $name() {
                let input = Cursor::new($str.as_bytes());
                let mut p = Parser::new(input.bytes());
                assert_eq!(Some($expected), p.parse_bool().unwrap().as_bool());
            }
            )*
        }
    }

    make_bool_tests!(true,
                     parse_true,       "true",
                     parse_true_upper, "True");
    make_bool_tests!(false,
                     parse_false,       "false",
                     parse_false_upper, "False");
    make_fail_tests!(parse_true_allupper,  "TRUE",
                     parse_false_allupper, "FALSE");


    macro_rules! make_list_test {
        ($name:ident, $value:ident, $str:expr, $block:block) => {
            #[test]
            fn $name() {
                let input = Cursor::new($str.as_bytes());
                let mut p = Parser::new(input.bytes());
                if let Some($value) = p.parse_list().unwrap().as_list() {
                    $block
                } else {
                    panic!("Parsing did not return a value");
                }
            }
        }
    }

    make_list_test!(list_empty, value, "[]", {
        assert_eq!(0, value.len());
    });

    make_list_test!(list_empty_space, value, "[  ]", {
        assert_eq!(0, value.len());
    });

    make_list_test!(list_one_item, value, "[true]", {
        assert_eq!(1, value.len());
        assert_eq!(Some(true), value[0].as_bool());
    });

    make_list_test!(list_two_items_comma, value, "[true,false]", {
        assert_eq!(2, value.len());
        assert_eq!(Some(true), value[0].as_bool());
        assert_eq!(Some(false), value[1].as_bool());
    });

    make_list_test!(list_two_items_space, value, "[true false]", {
        assert_eq!(2, value.len());
        assert_eq!(Some(true), value[0].as_bool());
        assert_eq!(Some(false), value[1].as_bool());
    });

    make_list_test!(list_two_items_commaspace, value, "[true, false]", {
        assert_eq!(2, value.len());
        assert_eq!(Some(true), value[0].as_bool());
        assert_eq!(Some(false), value[1].as_bool());
    });

    make_list_test!(list_two_items_spacecommaspace, value, "[true , false]", {
        assert_eq!(2, value.len());
        assert_eq!(Some(true), value[0].as_bool());
        assert_eq!(Some(false), value[1].as_bool());
    });

    make_fail_tests!(list_unterminated, "[",
                     list_unterminated_space, "[  ",
                     list_unterminated_one_item, "[true",
                     list_unterminated_one_item_space, "[true ",
                     list_unterminated_one_item_comma, "[true,",
                     list_unterminated_one_item_commaspace, "[true, ",
                     list_unterminated_one_item_spacecommaspace, "[true , ");


    macro_rules! make_dict_test {
        ($name:ident, $value:ident, $str:expr, $block:block) => {
            #[test]
            fn $name() {
                let input = Cursor::new($str.as_bytes());
                let mut p = Parser::new(input.bytes());
                if let Some($value) = p.parse_dict().unwrap().as_dict() {
                    $block
                } else {
                    panic!("Parsing did not return a value");
                }
            }
        }
    }

    make_dict_test!(dict_empty, value, "{}", {
        assert_eq!(0, value.len());
    });
    make_dict_test!(dict_empty_space, value, "{  }", {
        assert_eq!(0, value.len());
    });
    make_dict_test!(dict_one_item, value, "{ a: True }", {
        assert_eq!(1, value.len());
        assert!(value.contains_key("a"));
        assert_eq!(Some(true), value.get("a").unwrap().as_bool());
    });
    make_dict_test!(dict_one_item_space, value, "{a False}", {
        assert_eq!(1, value.len());
        assert!(value.contains_key("a"));
        assert_eq!(Some(false), value.get("a").unwrap().as_bool());
    });
    make_dict_test!(dict_two_items, value, "{a:true,b:false}", {
        assert_eq!(2, value.len());
        assert_eq!(true, value.get("a").unwrap().as_bool().unwrap());
        assert_eq!(false, value.get("b").unwrap().as_bool().unwrap());
    });
    make_dict_test!(dict_two_items_space, value, "{a:true b:false}", {
        assert_eq!(2, value.len());
        assert_eq!(true, value.get("a").unwrap().as_bool().unwrap());
        assert_eq!(false, value.get("b").unwrap().as_bool().unwrap());
    });
    make_dict_test!(dict_two_items_commaspace, value, "{a:true, b:false}", {
        assert_eq!(2, value.len());
        assert_eq!(true, value.get("a").unwrap().as_bool().unwrap());
        assert_eq!(false, value.get("b").unwrap().as_bool().unwrap());
    });

    make_fail_tests!(dict_unterminated, "{",
                     dict_unterminated_with_key, "{ key",
                     dict_unterminated_with_key_space, "{ key ",
                     dict_unterminated_with_key_colon, "{ key:",
                     dict_unterminated_with_key_colonspace, "{ key: ",
                     dict_missing_value, "{ key }",
                     dict_missing_value_colon, "{ key: }");


    macro_rules! make_string_tests {
        ($($name:ident, $str:expr, $expected:expr),+) => {
            $(
            #[test]
            fn $name() {
                let input = Cursor::new($str.as_bytes());
                let mut p = Parser::new(input.bytes());
                assert_eq!($expected, p.parse_string().unwrap().as_string().unwrap());
            }
            )*
        }
    }

    make_string_tests!(string_empty, "\"\"", "",
                       string_foo_bar, "\"foo bar\"", "foo bar",
                       string_unicode, "\"☺\"", "☺");

    macro_rules! make_number_tests {
        ($t:ident, $($name:ident, $str:expr, $expected:expr),+) => {
            $(
            #[test]
            fn $name() {
                let mut p = Parser::new(Cursor::new($str.as_bytes()).bytes());
                match p.parse_number().unwrap() {
                    Value::$t(value) => assert_eq!($expected, value),
                    _ => panic!("{} decoded into in incorrect type", stringify!($t)),
                }
            }
            )*
        }
    }

    make_number_tests!(Integer,
                       integer_zero, "0", 0,
                       integer_zero_octal, "00", 0,
                       integer_zero_hex, "0x0", 0,
                       integer_negative, "-42", -42,
                       integer_signed, "+42", 42,
                       integer_hex, "0xCAFE", 0xCAFE,
                       integer_octal, "01744", 0o1744,
                       integer_negative_oct, "-0644", -0o644);

    make_number_tests!(Float,
                       float_zero, "0.0", 0f64,
                       float_zero_exp, "0e0", 0f64,
                       float_zero_negative, "-0.0", -0f64,
                       float_exp_negative, "1e-0", 1e-0_f64,
                       float_negative_exp_negative, "-1e-0", -1e0_f64);

    macro_rules! make_exotic_float_tests {
        ($check:ident, $name:ident, $($str:expr),+) => {
            #[test]
            fn $name() {
                $({
                    println!($str);
                    let mut p = Parser::new(Cursor::new($str.as_bytes()).bytes());
                    assert!(p.parse_number().unwrap().as_float().unwrap().$check());
                })*
            }
        }
    }

    make_exotic_float_tests!(is_nan, float_nan, "NaN", "nan", "NAN", "nAn", "nAN", "Nan");
    make_exotic_float_tests!(is_infinite, float_inf, "Inf", "inf", "iNf", "iNF",
                             "Infinity", "infinity", "iNfInItY", "iNFiniTy");
}
