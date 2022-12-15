use super::Type;
use std::fmt::Display;

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        match self {
            Unit => write!(f, "()"),
            Int => write!(f, "int"),
            Float => write!(f, "float"),
            String => write!(f, "string"),
            Bool => write!(f, "bool"),
            Tuple(v) => {
                let mut t_str = "(".to_string();
                for i in 0..v.len() - 1 {
                    t_str.push_str(&format!("{}, ", v[i]));
                }

                t_str.push_str(&format!("{})", v[v.len() - 1]));
                write!(f, "{}", t_str)
            }
            List(t) => write!(f, "[{}]", t),
            Function(t1, t2) => write!(f, "{} -> {}", t1, t2),
            Delay(t) => write!(f, "@{}", t),
            Stable(t) => write!(f, "#{}", t),
            Fix(v, t) => match *t.clone() {
                Tuple(ts) => {
                    if ts.len() == 2 && matches!(*ts[1], FixVar(_)) {
                        write!(f, "Str<{}>", ts[0])
                    } else {
                        write!(f, "Fix {}.{}", v, t)
                    }
                }
                _ => write!(f, "Fix {}.{}", v, t),
            },
            FixVar(v) => write!(f, "{}", v),
            Generic(v, t) => {
                let mut vars = "∀ ".to_string();
                for i in 0..v.len() - 1 {
                    vars.push_str(&format!("{}, ", v[i]));
                }

                vars.push_str(&format!("{}", v[v.len() - 1]));
                write!(f, "{}.{}", vars, t)
            }
            GenericVar(v) => write!(f, "{}", v),
            Struct(map) => {
                let mut t_str = "struct {".to_string();

                for (s, t) in map {
                    t_str.push_str(&format!("{}: {}, ", s, t));
                }

                t_str.push('}');
                write!(f, "{}", t_str)
            }
            Enum(map) => {
                let mut t_str = "enum {".to_string();

                for (s, o) in map {
                    match o {
                        None => t_str.push_str(&format!("{}, ", s)),
                        Some(t) => t_str.push_str(&format!("{}({}), ", s, t)),
                    }
                }

                t_str.push('}');
                write!(f, "{}", t_str)
            }
        }
    }
}
