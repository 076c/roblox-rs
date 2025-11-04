// Visitor module for the Luau AST

use crate::ast::*;

pub struct LuauVisitor {
    pub ast: LuauAst,
}

impl LuauVisitor {
    pub fn new(ast: LuauAst) -> Self {
        Self { ast: ast }
    }
}
