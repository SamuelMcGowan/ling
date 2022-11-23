use crate::chunk::ConstIdx;

#[derive(Debug, Clone)]
pub(crate) struct Module {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Const(ConstIdx),
}
