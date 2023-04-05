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
    pub body: Block,
}

#[derive(Node!)]
pub(crate) enum Ty {
    Constructed { ident: Ident, params: Vec<Ty> },
    Unit,
}

#[derive(Node!)]
pub(crate) struct Block {
    pub stmts: Vec<Stmt>,
    pub eval_as_final: bool,
}

#[derive(Node!)]
pub(crate) enum Stmt {
    Expr(Expr),
    Dummy,
}

impl Stmt {
    pub fn expect_delim(&self) -> bool {
        match self {
            Self::Expr(expr) => expr.expect_delim(),

            // the recovery function should have recovered past any semicolons
            Self::Dummy => false,
        }
    }
}

#[derive(Node!)]
pub(crate) enum Expr {
    Const(ConstIdx),

    Infix {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },

    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },

    Var(Ident),

    Unit,
}

impl Expr {
    pub fn expect_delim(&self) -> bool {
        // every expression that we have for now expects a delimiter
        true
    }
}

#[derive(Node!, Copy)]
pub enum BinOp {
    LogicalAnd,
    LogicalOr,

    Equal,
    NotEqual,
    Gt,
    Lt,
    GtEqual,
    LtEqual,

    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Pow,
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
