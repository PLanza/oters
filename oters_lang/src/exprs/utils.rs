use super::{BOpcode, Expr, UOpcode};
use crate::parser::ast::Pattern;
use std::fmt::Display;

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expr::*;
        match self {
            Bool(b) => write!(f, "{}", b),
            Int(i) => write!(f, "{}", i),
            Float(fl) => write!(f, "{}", fl),
            String(s) => write!(f, "{}", s),
            Unit => write!(f, "()"),
            BinOp(e1, op, e2) => write!(f, "({} {} {})", e1.term, op, e2.term),
            UnOp(op, e) => write!(f, "{}({})", op, e.term),
            Delay(e) => write!(f, "@({})", e.term),
            Stable(e) => write!(f, "#({})", e.term),
            Adv(e) => write!(f, "!@({})", e.term),
            Unbox(e) => write!(f, "!#({})", e.term),
            Out(e) => write!(f, "out({})", e.term),
            Into(e) => write!(f, "into({})", e.term),
            List(v) => {
                if v.len() == 0 {
                    write!(f, "[]")
                } else {
                    let mut str = "[".to_string();
                    for i in 0..v.len() - 1 {
                        str.push_str(&format!("{}, ", v[i].term));
                    }

                    str.push_str(&format!("{}]", v[v.len() - 1].term));
                    write!(f, "{}", str)
                }
            }
            Tuple(v) => {
                let mut str = "(".to_string();
                for i in 0..v.len() - 1 {
                    str.push_str(&format!("{}, ", v[i].term));
                }

                str.push_str(&format!("{})", v[v.len() - 1].term));
                write!(f, "{}", str)
            }
            Struct(path, id, fields) => {
                let mut str = "".to_string();
                for module in path {
                    str.push_str(&format!("{}::", module));
                }
                str.push_str(id);
                str.push_str(" {");

                for i in 0..fields.len() {
                    str.push_str(&format!("{}: {}, ", fields[i].0, fields[i].1.term));
                }

                str.push('}');
                write!(f, "{}", str)
            }
            Variant(path, constr, o) => {
                let mut path_str = "".to_string();
                for module in path {
                    path_str.push_str(&format!("{}::", module));
                }
                match o {
                    None => write!(f, "{}{}", path_str, constr),
                    Some(e) => write!(f, "{}{}({})", path_str, constr, e.term),
                }
            }
            Fn(pat, e) => write!(f, "fn {} -> (\n{}\n)", pat.term, e.term),
            Fix(var, e) => write!(f, "fix {}.({})", var, e.term),
            If(e1, e2, e3) => write!(f, "if {} then\n{}\nelse\n{}", e1.term, e2.term, e3.term),
            Seq(e1, e2) => write!(f, "{}; \n{}", e1.term, e2.term),
            App(e1, e2) => write!(f, "{} {}", e1.term, e2.term),
            ProjStruct(e, field) => write!(f, "{}.{}", e.term, field),
            Match(e, v) => {
                let mut str = format!("match {} {{\n", e.term);

                for i in 0..v.len() {
                    str.push_str(&format!("{} => {},\n", v[i].0.term, v[i].1.term));
                }

                str.push('}');
                write!(f, "{}", str)
            }
            Var(path, x) => {
                let mut str = "".to_string();
                for module in path {
                    str.push_str(&format!("{}::", module));
                }
                str.push_str(x);
                write!(f, "{}", str)
            }
            LetIn(pat, e1, e2) => write!(f, "let {} = {} in\n{}", pat.term, e1.term, e2.term),
            Location(i) => write!(f, "loc {}", i),
        }
    }
}

impl Display for BOpcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BOpcode::*;
        match self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            Mod => write!(f, "%"),
            Cons => write!(f, "::"),
            Eq => write!(f, "=="),
            Lt => write!(f, "<"),
            Gt => write!(f, ">"),
            And => write!(f, "&&"),
            Or => write!(f, "||"),
        }
    }
}

impl Display for UOpcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use UOpcode::*;
        match self {
            Neg => write!(f, "~"),
            Not => write!(f, "!"),
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Pattern::*;
        match self {
            Underscore => write!(f, "_"),
            Bool(b) => write!(f, "{}", b),
            Unit => write!(f, "()"),
            Int(i) => write!(f, "{}", i),
            Float(fl) => write!(f, "{}", fl),
            String(s) => write!(f, "{}", s),
            List(v) => {
                if v.len() == 0 {
                    return write!(f, "[]");
                }

                let mut str = "[".to_string();
                for i in 0..v.len() - 1 {
                    str.push_str(&format!("{}, ", v[i].term));
                }

                str.push_str(&format!("{}]", v[v.len() - 1].term));
                write!(f, "{}", str)
            }
            Tuple(v) => {
                let mut str = "(".to_string();
                for i in 0..v.len() - 1 {
                    str.push_str(&format!("{}, ", v[i].term));
                }

                str.push_str(&format!("{})", v[v.len() - 1].term));
                write!(f, "{}", str)
            }
            Struct(path, id, fields) => {
                let mut str = "".to_string();
                for module in path {
                    str.push_str(&format!("{}::", module));
                }
                str.push_str(id);
                str.push_str(" {");

                for i in 0..fields.len() {
                    match &fields[1].1 {
                        None => str.push_str(&format!("{}, ", fields[i].0)),
                        Some(p) => str.push_str(&format!("{}: {}, ", fields[i].0, p.term)),
                    }
                }

                str.push('}');
                write!(f, "{}", str)
            }
            Variant(path, constr, o) => {
                let mut path_str = "".to_string();
                for module in path {
                    path_str.push_str(&format!("{}::", module));
                }
                match o {
                    None => write!(f, "{}{}", path_str, constr),
                    Some(e) => write!(f, "{}{}({})", path_str, constr, e.term),
                }
            }
            Cons(p1, p2) => write!(f, "{} :: {}", p1.term, p2.term),
            Stream(p1, p2) => write!(f, "{} << {}", p1.term, p2.term),
            Or(p1, p2) => write!(f, "{} || {}", p1.term, p2.term),
            Var(x, b) => {
                if *b {
                    write!(f, "#{}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
        }
    }
}
