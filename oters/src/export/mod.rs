mod errors;

use std::collections::HashMap;

pub use self::errors::*;
use crate::exprs::Expr;
use crate::types::Type;
pub use oters_macro::{export_list, export_oters};

use anyhow::Result;

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Unit,
    List(Vec<Box<Value>>),
    Tuple(Vec<Box<Value>>),
    // Fn
    Struct(String, HashMap<String, Box<Value>>),
    // Enum
}

pub type ExportFns = HashMap<String, (fn(Vec<Value>) -> Value, Vec<Type>, Type)>;
pub type ExportStructs = Vec<(String, HashMap<String, Box<Type>>)>;

impl Value {
    pub fn expr_to_args(e: Expr, args_len: usize) -> Result<Vec<Value>> {
        if args_len == 1 {
            Ok(vec![Value::from_expr(e.clone())?])
        } else {
            match e {
                Expr::Tuple(vals) => {
                    let mut args = Vec::new();
                    for val in vals.into_iter() {
                        args.push(Value::from_expr(*val)?);
                    }
                    Ok(args)
                }
                _ => Err(ValueError::ImproperArguments(e).into()),
            }
        }
    }

    pub fn from_expr(e: Expr) -> Result<Value> {
        use Expr::*;
        match e {
            Bool(b) => Ok(Value::Bool(b)),
            Int(i) => Ok(Value::Int(i)),
            Float(f) => Ok(Value::Float(f)),
            String(s) => Ok(Value::String(s)),
            Unit => Ok(Value::Unit),
            List(vals) => {
                let mut list = Vec::new();
                for val in vals {
                    list.push(Box::new(Value::from_expr(*val)?));
                }
                Ok(Value::List(list))
            }
            Tuple(vals) => {
                let mut tuple = Vec::new();
                for val in vals {
                    tuple.push(Box::new(Value::from_expr(*val)?));
                }
                Ok(Value::Tuple(tuple))
            }
            Struct(name, fields) => {
                let mut strct = HashMap::new();
                for (field, val) in fields {
                    strct.insert(field, Box::new(Value::from_expr(*val)?));
                }
                Ok(Value::Struct(name, strct))
            }
            _ => Err(ValueError::InvalidConversion(e).into()),
        }
    }

    pub fn to_expr(self) -> Expr {
        use Value::*;
        match self {
            Unit => Expr::Unit,
            Bool(b) => Expr::Bool(b),
            Int(i) => Expr::Int(i),
            Float(f) => Expr::Float(f),
            String(s) => Expr::String(s),
            List(vals) => Expr::List(vals.into_iter().map(|v| Box::new(v.to_expr())).collect()),
            Tuple(vals) => Expr::Tuple(vals.into_iter().map(|v| Box::new(v.to_expr())).collect()),
            Struct(name, fields) => Expr::Struct(
                name,
                fields
                    .into_iter()
                    .map(|(f, v)| (f, Box::new(v.to_expr())))
                    .collect(),
            ),
        }
    }
}
