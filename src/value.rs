#[derive(Debug, Clone)]
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
}
