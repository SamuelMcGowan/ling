use macro_rules_attribute::{derive, derive_alias};
use ustr::Ustr;

use crate::constants::ConstIdx;

derive_alias! {
    #[derive(Node!)] = #[derive(
        Debug,
        Clone,
        serde::Serialize,
    )];
}

#[derive(Node!)]
pub(crate) struct Module {
    pub items: Vec<Item>,
}

#[derive(Node!)]
pub(crate) enum Item {
    Func(Func),
    Dummy,
}

#[derive(Node!)]
pub(crate) struct Func {
    pub ident: Ident,
    pub params: Vec<(Ident, Ty)>,
    pub ret_ty: Ty,
    pub body: Expr,
}

#[derive(Node!)]
pub(crate) enum Ty {
    Constructed { ident: Ident, params: Vec<Ty> },
    Unit,
}

#[derive(Node!)]
pub(crate) enum Stmt {
    Expr(Expr),
}

#[derive(Node!)]
pub(crate) enum Expr {
    Const(ConstIdx),
    Unit,
}

#[derive(Node!)]
pub(crate) enum Ident {
    Unresolved(Ustr),
}

impl Ident {
    pub fn unresolved(&self) -> Option<Ustr> {
        match self {
            Self::Unresolved(s) => Some(*s),
        }
    }
}
