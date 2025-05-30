use core::fmt::Display;

#[derive(PartialEq)]
pub enum LoxValue {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl LoxValue {
    pub fn is_truthy(&self) -> bool {
        match self {
            LoxValue::Nil => false,
            LoxValue::Boolean(b) => *b,
            // Everything else is truthy
            _ => true,
        }
    }
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Boolean(b) => write!(f, "{}", b),
            LoxValue::Number(n) => {
                let repr = format!("{}", n);
                if let Some(repr) = repr.strip_suffix(".0") {
                    write!(f, "{}", repr)
                } else {
                    write!(f, "{}", repr)
                }
            },
            LoxValue::String(s) => write!(f, "{:?}", s),
        }
    }
}