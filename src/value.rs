use ustr::Ustr;

#[derive(Clone)]
pub(crate) enum Value {
    String(Ustr),
    Integer(u64),
    Float(f64),
    Bool(bool),
}

impl Value {
    pub fn is_equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => (a - b).abs() <= f64::EPSILON,
            (Self::Bool(a), Self::Bool(b)) => a == b,
            _ => false,
        }
    }

    pub fn repr(&self) -> String {
        match self {
            // TODO: use custom implementation of string printing
            Self::String(s) => format!("{:?}", s.as_str()),
            Self::Integer(n) => format!("{n}"),
            Self::Float(n) => format!("{n}"),
            Self::Bool(b) => format!("{b:?}")
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Value({})", self.repr())
    }
}
