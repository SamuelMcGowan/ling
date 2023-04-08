use crate::syntax::ast::*;

pub(crate) trait Visitor {
    fn visit_module(&mut self, module: &mut Module) {
        self.walk_module(module);
    }
    fn walk_module(&mut self, module: &mut Module) {
        for item in &mut module.items {
            match item {
                Item::Func(func) => self.visit_func(func),
                Item::Dummy => {}
            }
        }
    }

    fn visit_func(&mut self, func: &mut Func);

    fn visit_block(&mut self, block: &mut Block) {
        self.walk_block(block);
    }
    fn walk_block(&mut self, block: &mut Block) {
        for stmt in &mut block.stmts {
            self.visit_stmt(stmt);
        }
        self.visit_expr(&mut block.final_expr);
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        self.walk_stmt(stmt);
    }
    fn walk_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Expr(expr) => self.visit_expr(expr),

            Stmt::Loop(block) => self.visit_loop(block),
            Stmt::WhileLoop { cond, block } => self.visit_while_loop(cond, block),

            Stmt::Declaration { lhs, rhs } => self.visit_declaration(lhs, rhs),
            Stmt::Assignment { lhs, rhs } => self.visit_assignment(lhs, rhs),

            Stmt::Dummy => {}
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        self.walk_expr(expr);
    }
    fn walk_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Const(_) => {}
            Expr::UnaryOp { op: _, expr } => self.visit_expr(expr),
            Expr::BinOp { op: _, lhs, rhs } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            Expr::Call { callee, args } => {
                self.visit_expr(callee);
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Expr::If { branches, else_ } => {
                for branch in branches {
                    self.visit_expr(&mut branch.cond);
                    self.visit_block(&mut branch.then);
                }
                if let Some(block) = else_ {
                    self.visit_block(block);
                }
            }
            Expr::Var(var) => self.visit_var(var),

            Expr::Unit => {}

            Expr::Dummy => {}
        }
    }

    fn visit_declaration(&mut self, lhs: &mut Ident, rhs: &mut Expr);

    fn visit_assignment(&mut self, lhs: &mut Var, rhs: &mut Expr) {
        self.visit_var(lhs);
        self.visit_expr(rhs);
    }

    fn visit_var(&mut self, var: &mut Var);

    fn visit_loop(&mut self, block: &mut Block) {
        self.visit_block(block)
    }

    fn visit_while_loop(&mut self, cond: &mut Expr, block: &mut Block) {
        self.visit_expr(cond);
        self.visit_block(block);
    }
}
