use proc_macro2::Span;

///! IR Transpiling
///
/// Converts Custom IR to a Luau AST
/// Part of roblox-rs
// Module imports
use crate::ast::*;
use crate::error::*;
use crate::ir::*;
use crate::transpiler;

pub fn transpile_expr(expr: IRExpr) -> LuauExpression {
    match expr {
        IRExpr::Null => LuauExpression::Nil,
        IRExpr::Bool(bool) => LuauExpression::Boolean(bool),
        IRExpr::Number(num) => LuauExpression::Number(num),
        IRExpr::String(str) => LuauExpression::String(str),
        IRExpr::Char(char) => LuauExpression::String(char),
        IRExpr::ByteChar(char) => LuauExpression::String(char),
        IRExpr::ByteString(bytes) => LuauExpression::String(String::from_utf8(bytes).unwrap()),
        IRExpr::CString(str) => LuauExpression::String(str),
        IRExpr::RawString(str) => LuauExpression::String(str),
        IRExpr::Identifier(ident) => LuauExpression::Identifier(ident),
        IRExpr::Member { base, name } => {
            let base = transpile_expr(*base);
            LuauExpression::Member {
                base: Box::new(base),
                member: name,
            }
        }
        IRExpr::Call { callee, arguments } => {
            let callee = transpile_expr(*callee);
            let arguments = arguments
                .iter()
                .map(|arg| transpile_expr(arg.clone()))
                .collect::<Vec<_>>();
            LuauExpression::FunctionCall {
                callee: Box::new(callee),
                arguments: arguments,
                method: None,
            }
        }
        IRExpr::MethodCall {
            callee,
            method,
            arguments,
        } => {
            let callee = transpile_expr(*callee);
            let arguments = arguments
                .iter()
                .map(|arg| transpile_expr(arg.clone()))
                .collect::<Vec<_>>();

            LuauExpression::FunctionCall {
                callee: Box::new(callee),
                arguments: arguments,
                method: Some(method.clone()),
            }
        }
        IRExpr::Array(elements) => {
            let elements = elements
                .iter()
                .map(|el| transpile_expr(el.clone()))
                .collect::<Vec<_>>();
            LuauExpression::Table(
                elements
                    .into_iter()
                    .map(|el| LuauTableEntry::Unnamed(el))
                    .collect(),
            )
        }
        IRExpr::Path(path) => {
            let mut expr = LuauExpression::Identifier(
                path.get(0).expect("Expected starting path").to_string(),
            );

            // TODO: don't use clone?
            for (index, sub_path) in path.clone().split_off(1).iter().enumerate() {
                expr = LuauExpression::Member {
                    base: Box::new(expr),
                    member: sub_path.to_string(),
                };
            }

            expr
        }
        IRExpr::BinaryOperation {
            left,
            operator,
            right,
        } => LuauExpression::BinaryOperation {
            left: Box::new(transpile_expr(*left)),
            operator: match operator {
                IRBinOp::Div => LuauBinOp::Div,
                IRBinOp::Plus => LuauBinOp::Add,
                IRBinOp::Minus => LuauBinOp::Sub,
                IRBinOp::Mul => LuauBinOp::Mul,
                IRBinOp::Eq => LuauBinOp::Eq,
                IRBinOp::Ne => LuauBinOp::Ne,
                IRBinOp::Ge => LuauBinOp::Ge,
                IRBinOp::Le => LuauBinOp::Le,
                IRBinOp::Lt => LuauBinOp::Lt,
                IRBinOp::LogAnd => LuauBinOp::And,
                IRBinOp::LogOr => LuauBinOp::Or,
                IRBinOp::Rem => LuauBinOp::Mod,
                _ => panic!("TODO: {:#?}", operator),
            },
            right: Box::new(transpile_expr(*right)),
        },
        _ => panic!("TODO: {:#?}", expr),
    }
}

pub fn transpile_ty(ty: &IRType) -> DiagResult<Option<LuauType>> {
    match ty {
        IRType::None => Ok(None), // TODO: Some(LuauType::Unknown) ??
        IRType::Infer => Ok(None),
        IRType::SimpleType { base } => Ok(Some(LuauType::Name(base.to_string()))),
        IRType::Member { base, member } => Ok(Some(LuauType::MemberAccess {
            base: Box::new(transpile_ty(base)?.unwrap()),
            field: member.to_string(),
        })),
        IRType::TypeFunction { caller, args } => Ok(Some(LuauType::TypeFunction {
            caller: caller.to_string(),
            args: args
                .iter()
                .map(|a| Box::new(transpile_ty(&a).expect("Failed to convert type!").unwrap()))
                .collect::<Vec<_>>(),
        })),
        _ => todo!(),
    }
}

pub fn transpile_ident(ident: &IRIdent) -> DiagResult<LuauBinding> {
    match ident {
        IRIdent::Wild => Ok(LuauBinding::Identifier {
            name: "_".into(),
            var_type: None,
        }),
        IRIdent::Tuple { idents } => Ok(LuauBinding::VarList(
            idents
                .clone()
                .iter()
                .map(|c| transpile_ident(&c.clone()).unwrap())
                .collect::<Vec<_>>(),
        )),
        IRIdent::Identifier {
            name,
            mutable,
            ty,
            init,
        } => Ok(LuauBinding::Identifier {
            name: name.to_string(),
            var_type: ty
                .clone()
                .and_then(|_| transpile_ty(&ty.clone().unwrap()).expect("Failed to lift type")),
        }),
    }
}

pub fn transpile_stmt(stmt: IRStmt) -> DiagResult<Vec<LuauStatement>> {
    match stmt {
        IRStmt::Semi(expr) => match expr {
            IRExpr::Call { .. } => Ok(vec![LuauStatement::FunctionCall(transpile_expr(expr))]),
            _ => Err(Diagnostic::new(
                "Expression cannot stand.",
                Span::call_site(),
            )),
        },
        IRStmt::FnDecl {
            name,
            parameters,
            return_type,
            body,
            vis,
        } => {
            let parameters = parameters
                .iter()
                .map(|param| match param {
                    IRIdent::Identifier { name, .. } => name.clone(),
                    _ => todo!(),
                })
                .collect::<Vec<_>>();

            let body = body
                .iter()
                .map(|stmt| transpile_stmt(*stmt.clone()))
                .collect::<Result<Vec<_>, _>>()?
                .concat();

            Ok(vec![LuauStatement::FunctionDecl {
                name: vec![name],
                parameters: parameters
                    .into_iter()
                    .map(|param| LuauBinding::Identifier {
                        name: param,
                        var_type: None,
                    })
                    .collect(),
                vararg: None,
                body: body,
                is_method: false,
                generics: None,
                // TODO: left off here, continue later
                return_type: transpile_ty(&return_type)?,
                attributes: Vec::new(),
            }])
        }
        IRStmt::Let { variable, expr } => Ok(vec![LuauStatement::Assignment {
            variables: transpile_ident(&variable)?,
            values: vec![transpile_expr(expr)],
        }]),
        _ => panic!("TODO: {:#?}", stmt),
    }
}

pub fn transpile_ir(ir: Vec<IRStmt>) -> DiagResult<Vec<LuauStatement>> {
    let mut statements = Vec::new();

    for stmt in ir {
        statements.append(&mut transpile_stmt(stmt).expect("Failed to lift statements."));
    }

    Ok(statements)
}
