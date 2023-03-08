use ustr::Ustr;

use crate::constants::ConstIdx;

#[derive(Debug, Clone)]
pub(crate) struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub(crate) enum Item {
    Func(Func),
}

#[derive(Debug, Clone)]
pub(crate) struct Func {
    pub ident: Ident,
    pub params: Vec<(Ident, Ty)>,
    pub ret_ty: Ty,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub(crate) enum Ty {
    Constructed { ident: Ident, params: Vec<Ty> },
    Unit,
}

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Const(ConstIdx),
    Unit,
}

#[derive(Debug, Clone)]
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
