use macro_rules_attribute::{derive, derive_alias};
use ustr::Ustr;

use crate::constants::ConstIdx;
use crate::symbol_table::SymbolId;

use super::parser::ParseResult;
use super::source::Span;

derive_alias! {
    #[derive(Node!)] = #[derive(
        Debug,
        Clone,
        serde::Serialize,
    )];
}

pub(crate) struct Spanned<T> {
    pub(crate) inner: T,
    pub(crate) span: Option<Span>,
}

impl<T> Spanned<ParseResult<T>> {
    pub fn transpose(self) -> ParseResult<Spanned<T>> {
        self.inner.map(|inner| Spanned {
            inner,
            span: self.span,
        })
    }
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
    pub final_expr: Expr,
}

#[derive(Node!)]
pub(crate) enum Stmt {
    Expr(Expr),

    Loop(Block),
    WhileLoop { cond: Expr, block: Block },

    Declaration { lhs: Ident, rhs: Expr },
    Assignment { lhs: Var, rhs: Expr },

    Dummy,
}

impl Stmt {
    pub fn expect_delim(&self) -> bool {
        match self {
            Self::Expr(expr) => expr.expect_delim(),

            Self::Loop(_) | Self::WhileLoop { .. } => false,

            Self::Declaration { .. } => true,
            Self::Assignment { .. } => true,

            // the recovery function should have recovered past any semicolons
            Self::Dummy => false,
        }
    }
}

#[derive(Node!)]
pub(crate) enum Expr {
    Const(ConstIdx),

    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },

    BinOp {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },

    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },

    If {
        branches: Vec<IfBranch>,
        else_: Option<Box<Block>>,
    },

    Var(Var),

    Unit,

    Dummy,
}

impl Expr {
    pub fn expect_delim(&self) -> bool {
        !matches!(self, Self::If { .. })
    }
}

#[derive(Node!, Copy)]
pub enum UnaryOp {
    Neg,
    Not,
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

    Call,
    Access,
}

#[derive(Node!)]
pub(crate) struct IfBranch {
    pub(crate) cond: Expr,
    pub(crate) then: Block,
}

#[derive(Node!)]
pub(crate) enum Var {
    Simple(Ident),
    Field { expr: Box<Expr>, field: Ident },
}

#[derive(Node!)]
pub(crate) enum Ident {
    Unresolved(Ustr),
    Resolved(SymbolId),
}

impl Ident {
    pub fn unresolved(&self) -> Option<Ustr> {
        match self {
            Self::Unresolved(s) => Some(*s),
            Self::Resolved(_) => None,
        }
    }
}
