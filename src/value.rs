//
// value.rs
// Copyright (C) 2015 Adrian Perez <aperez@igalia.com>
// Distributed under terms of the MIT license.
//

use std::collections::BTreeMap;


#[derive(Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String),
    List(Vec<Value>),
    Dict(BTreeMap<String, Value>),
}


impl Value {
    /// Returns true if the `Value` is a bool, otherwise returns false.
    pub fn is_bool(&self) -> bool {
        match self {
            &Value::Bool(_) => true,
            _ => false,
        }
    }

    /// If the `Value` is a boolean, return the associated bool. Returns None otherwise.
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            &Value::Bool(b) => Some(b),
            _ => None,
        }
    }

    /// Returns true if `Value` is an integer, otherwise returns false.
    pub fn is_integer(&self) -> bool {
        match self {
            &Value::Integer(_) => true,
            _ => false,
        }
    }

    /// If the `Value` is an integer, return the associated i64. Returns None otherwise.
    pub fn as_integer(&self) -> Option<i64> {
        match self {
            &Value::Integer(i) => Some(i),
            _ => None,
        }
    }

    /// Returns true if `Value` is a floating point number, otherwise returns false.
    pub fn is_float(&self) -> bool {
        match self {
            &Value::Float(_) => true,
            _ => false,
        }
    }

    /// If the `Value` is a floating point number, return the associated f64. Returns None
    /// otherwise.
    pub fn as_float(&self) -> Option<f64> {
        match self {
            &Value::Float(f) => Some(f),
            _ => None,
        }
    }

    /// Returns true if `Value` is a number (either integer or floating point), otherwise returns
    /// false.
    pub fn is_number(&self) -> bool {
        match self {
            &Value::Integer(_) => true,
            &Value::Float(_) => true,
            _ => false,
        }
    }

    /// If the `Value` is numeric, returns it as a f64. Return None otherwise.
    pub fn as_number(&self) -> Option<f64> {
        match self {
            &Value::Integer(i) => Some(i as f64),
            &Value::Float(f) => Some(f),
            _ => None,
        }
    }

    /// Returns true if `Value` is a String, otherwise returns false.
    pub fn is_string(&self) -> bool {
        match self {
            &Value::String(_) => true,
            _ => false,
        }
    }

    /// If the `Value` is a string, returns the associated String. Returns None otherwise.
    pub fn as_string(&self) -> Option<&str> {
        match self {
            &Value::String(ref s) => Some(s),
            _ => None,
        }
    }

    /// Returns true if `Value` is a List, otherwise returns false.
    pub fn is_list(&self) -> bool {
        match self {
            &Value::List(_) => true,
            _ => false,
        }
    }

    /// If the `Value` is a list, returns the associated Vec<Value>. Returns None otherwise.
    pub fn as_list(&self) -> Option<&Vec<Value>> {
        match self {
            &Value::List(ref l) => Some(l),
            _ => None,
        }
    }

    /// Return true if `Value` is a Dict, otherwise returns false.
    pub fn is_dict(&self) -> bool {
        match self {
            &Value::Dict(_) => true,
            _ => false,
        }
    }

    /// If the `Value` is a dictionary, returns the associated BTreeMap<String, Value>.
    /// Returns None otherwise.
    pub fn as_dict(&self) -> Option<&BTreeMap<String, Value>> {
        match self {
            &Value::Dict(ref d) => Some(d),
            _ => None,
        }
    }
}
