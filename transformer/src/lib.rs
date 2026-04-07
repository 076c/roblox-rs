///! IR lifting
///
/// Converts Syn IR to custom IR
/// Part of roblox-rs
// Module imports
pub mod ast;
pub mod error;
pub mod ir;
pub mod transpiler;

use proc_macro2::Span;
use syn::spanned::Spanned;
use syn::{File, GenericArgument, Ident, Local, LocalInit, Pat, PathArguments, PathSegment, Type};

use crate::error::*;
use crate::ir::*;

/// Creates a new Error
fn err<T>(msg: impl Into<String>, span: proc_macro2::Span) -> DiagResult<T> {
    Err(Diagnostic::new(msg, span))
}

/// Creates a new Diagnostic
fn diag(msg: &str, span: Span) -> Diagnostic {
    Diagnostic::new(msg, span)
}

/// Parses Rust source code using `Syn`
pub fn parse_source(source: String) -> Result<File, syn::Error> {
    syn::parse_str::<File>(&source)
}

/// Lift a Syn `Ident` into a string
pub fn lift_ident(ident: &Ident) -> DiagResult<String> {
    Ok(ident.to_string())
}

/// Lifts a Syn `GenericArgument` into a type
pub fn lift_generic_argument(arg: &GenericArgument) -> DiagResult<IRType> {
    match arg {
        GenericArgument::Type(ty) => Ok(lift_ty(ty)?),
        _ => todo!(),
    }
}

/// Lifts a Syn `PathArguments` into a Vec
pub fn lift_path_arguments_ty(args: &PathArguments) -> DiagResult<Vec<IRType>> {
    match args {
        PathArguments::AngleBracketed(args) => Ok(args
            .args
            .iter()
            .map(|el| lift_generic_argument(el))
            .collect::<Result<Vec<_>, _>>())?,
        _ => todo!(),
    }
}

/// Lifts a Syn `PathSegment` into a type
pub fn lift_path_segment_ty(segment: &PathSegment) -> DiagResult<IRType> {
    match segment.arguments.is_empty() {
        true => Ok(IRType::SimpleType {
            base: lift_ident(&segment.ident)?,
        }),
        false => Ok(IRType::TypeFunction {
            caller: lift_ident(&segment.ident)?,
            args: lift_path_arguments_ty(&segment.arguments)?
                .into_iter()
                .map(Box::new)
                .collect::<Vec<_>>(),
        }),
    }
}

/// Lifts a Syn `PathSegment` into an expression
pub fn lift_path_segment(segment: &PathSegment) -> DiagResult<String> {
    Ok(segment.ident.to_string())
}

/// Lifts a Syn `Type` into a type specifier
pub fn lift_ty(ty: &Type) -> DiagResult<IRType> {
    match ty {
        Type::Infer(_) => Ok(IRType::Infer),
        Type::Array(array) => Ok(IRType::Array {
            element_type: Box::new(lift_ty(&array.elem)?),
            len: 0f64, // TODO: lift expr
        }),
        Type::BareFn(bare_fn) => {
            let mut input_args = Vec::new();

            for element in &bare_fn.inputs {
                let name = element.name.as_ref().map(|tuple| tuple.0.to_string());
                let ty = Box::new(lift_ty(&element.ty)?);

                input_args.push((name, ty));
            }

            Ok(IRType::Function {
                input_args,
                return_type: match &bare_fn.output {
                    syn::ReturnType::Default => None,
                    syn::ReturnType::Type(_, ty) => Some(Box::new(lift_ty(&ty)?)),
                },
            })
        }
        Type::Path(path) => {
            let mut segments = path.path.segments.iter();

            let first = segments
                .next()
                .ok_or_else(|| diag("Empty path", path.span()))?;

            let mut current = lift_path_segment_ty(first)?;

            for segment in segments {
                current = IRType::TypeFunction {
                    caller: match current {
                        IRType::SimpleType { base } => base,
                        IRType::TypeFunction { caller, .. } => caller,
                        _ => return err("Invalid path caller", segment.ident.span()),
                    },
                    args: lift_path_arguments_ty(&segment.arguments)?
                        .into_iter()
                        .map(Box::new)
                        .collect(),
                };
            }

            Ok(current)
        }
        Type::Paren(ty) => Ok(IRType::Paren {
            ty: Box::new(lift_ty(&ty.elem)?),
        }),
        Type::Ptr(ptr) => err("Raw pointers are not allowed", ptr.star_token.span),
        Type::Reference(reference) => err("References are not allowed", reference.and_token.span),
        _ => todo!(),
    }
}

/// Lifts a Syn `Pat` into an identifier
pub fn lift_pat(pat: &Pat, ty: Option<&Type>) -> DiagResult<IRIdent> {
    match pat {
        Pat::Ident(ident) => Ok(IRIdent::Identifier {
            name: ident.ident.to_string(),
            mutable: ident.mutability.is_some(),
            ty: match ty {
                None => None,
                Some(ty) => Some(lift_ty(ty)?),
            },
            // TODO: init
            init: None,
        }),
        Pat::Type(pat_ty) => {
            let lifted_pat = lift_pat(&pat_ty.pat, Some(&pat_ty.ty))?;

            return Ok(lifted_pat);
        }
        Pat::Tuple(tuple) => Ok(IRIdent::Tuple {
            idents: tuple
                .elems
                .iter()
                .map(|el| lift_pat(&el, None))
                .collect::<Result<Vec<_>, _>>()?,
        }),
        _ => panic!("Invalid pattern"),
    }
}

/// Lifts a Syn `Member` into a string
pub fn lift_member(member: &syn::Member) -> DiagResult<String> {
    match member {
        syn::Member::Named(ident) => Ok(ident.to_string()),
        syn::Member::Unnamed(index) => Ok(index.index.to_string()),
    }
}

/// Lifts a Syn `Expr` into an expression
pub fn lift_expr(expr: &syn::Expr) -> DiagResult<IRExpr> {
    match expr {
        syn::Expr::Call(call) => {
            let caller = lift_expr(&call.func);

            let arguments = call
                .args
                .iter()
                .map(|arg| lift_expr(arg))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(IRExpr::Call {
                callee: Box::new(caller?),
                arguments: arguments,
            })
        }
        syn::Expr::Array(array) => {
            let elements = array
                .elems
                .iter()
                .map(|el| lift_expr(el))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(IRExpr::Array(elements))
        }
        syn::Expr::Lit(lit) => match &lit.lit {
            syn::Lit::Bool(bool) => Ok(IRExpr::Bool(bool.value)),
            syn::Lit::Int(int) => Ok(IRExpr::Number(int.base10_parse::<f64>().unwrap() as f64)),
            syn::Lit::Float(float) => {
                Ok(IRExpr::Number(float.base10_parse::<f64>().unwrap() as f64))
            }
            syn::Lit::Str(str) => Ok(IRExpr::String(str.value())),
            syn::Lit::Char(char) => Ok(IRExpr::Char(char.value().into())),
            syn::Lit::ByteStr(byte_str) => Ok(IRExpr::ByteString(byte_str.value())),
            syn::Lit::CStr(c_str) => Ok(IRExpr::CString(c_str.value().to_str().unwrap().into())),
            _ => todo!(),
        },
        syn::Expr::Binary(bin) => {
            let left = lift_expr(&bin.left)?;
            let right = lift_expr(&bin.right)?;

            Ok(IRExpr::BinaryOperation {
                left: Box::new(left),
                operator: match bin.op {
                    syn::BinOp::Add(_) => IRBinOp::Plus,
                    syn::BinOp::Sub(_) => IRBinOp::Minus,
                    syn::BinOp::Mul(_) => IRBinOp::Mul,
                    syn::BinOp::Div(_) => IRBinOp::Div,
                    syn::BinOp::Rem(_) => IRBinOp::Rem,
                    syn::BinOp::And(_) => IRBinOp::LogAnd,
                    syn::BinOp::Or(_) => IRBinOp::LogOr,
                    syn::BinOp::BitXor(_) => IRBinOp::BitXor,
                    syn::BinOp::BitAnd(_) => IRBinOp::BitAnd,
                    syn::BinOp::BitOr(_) => IRBinOp::BitOr,
                    syn::BinOp::Shl(_) => IRBinOp::BitShl,
                    syn::BinOp::Shr(_) => IRBinOp::BitShr,
                    syn::BinOp::Eq(_) => IRBinOp::Eq,
                    syn::BinOp::Ne(_) => IRBinOp::Ne,
                    syn::BinOp::Lt(_) => IRBinOp::Lt,
                    syn::BinOp::Le(_) => IRBinOp::Le,
                    syn::BinOp::Gt(_) => IRBinOp::Gt,
                    syn::BinOp::Ge(_) => IRBinOp::Ge,
                    _ => todo!(),
                },
                right: Box::new(right),
            })
        }
        syn::Expr::Tuple(tuple) => Ok(IRExpr::Tuple(
            tuple
                .elems
                .iter()
                .map(|expr| lift_expr(expr))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        syn::Expr::Path(path) => {
            let segments = path
                .path
                .segments
                .iter()
                .map(|seg| lift_path_segment(seg))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(IRExpr::Path(segments))
        }
        syn::Expr::Closure(closure) => {
            let parameters = closure
                .inputs
                .iter()
                .map(|param| lift_pat(&param, None))
                .collect::<Result<Vec<_>, _>>()?;

            let body = lift_expr(&closure.body)?;

            Ok(IRExpr::Closure {
                parameters: parameters,
                body: Box::new(body),
                is_move: closure.movability.is_some(),
                return_type: Some(lift_return_type(&closure.output)?),
            })
        }
        syn::Expr::Paren(paren) => Ok(IRExpr::Paren(Box::new(lift_expr(&paren.expr)?))),
        syn::Expr::Block(block) => Ok(IRExpr::Block {
            statements: block
                .block
                .stmts
                .iter()
                .map(|stmt| lift_stmt(stmt))
                .collect::<Result<Vec<_>, _>>()?,
            expr: None,
        }),
        syn::Expr::Try(expr) => Ok(IRExpr::Try(Box::new(lift_expr(&expr.expr)?))),
        syn::Expr::Await(expr) => Ok(IRExpr::Await(Box::new(lift_expr(&expr.base)?))),
        syn::Expr::MethodCall(method_call) => {
            let callee = lift_expr(&method_call.receiver)?;
            let method = method_call.method.to_string();

            let arguments = method_call
                .args
                .iter()
                .map(|arg| lift_expr(&arg))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(IRExpr::MethodCall {
                callee: Box::new(callee),
                method: method,
                arguments: arguments,
            })
        }
        syn::Expr::Field(field_expr) => Ok(IRExpr::Member {
            base: Box::new(lift_expr(&field_expr.base)?),
            name: lift_member(&field_expr.member)?,
        }),
        syn::Expr::Index(index_expr) => Ok(IRExpr::Index {
            base: Box::new(lift_expr(&index_expr.expr)?),
            index: Box::new(lift_expr(&index_expr.index)?),
        }),
        syn::Expr::If(if_expr) => {
            let condition = lift_expr(&if_expr.cond)?;
            let then_branch = lift_block(&if_expr.then_branch)?;

            let else_branch = match &if_expr.else_branch {
                None => None,
                Some(expr) => Some(Box::new(lift_expr(&expr.1)?)),
            };

            Ok(IRExpr::IfExpression {
                condition: Box::new(condition),
                then_clause: Box::new(IRExpr::Block {
                    statements: then_branch,
                    expr: None,
                }),
                else_clause: else_branch,
            })
        }
        syn::Expr::While(while_loop) => {
            let condition = lift_expr(&while_loop.cond)?;
            let body = lift_block(&while_loop.body)?;

            Ok(IRExpr::While {
                condition: Box::new(condition),
                body: Box::new(IRExpr::Block {
                    statements: body,
                    expr: None,
                }),
            })
        }
        syn::Expr::Cast(cast) => Ok(IRExpr::Cast {
            expr: Box::new(lift_expr(&cast.expr)?),
            ty: Box::new(lift_ty(&cast.ty)?),
        }),
        syn::Expr::ForLoop(for_loop) => Ok(IRExpr::ForLoop {
            iterator: Box::new(lift_expr(&for_loop.expr)?),
            variable: lift_pat(&for_loop.pat, None)?,
            body: Box::new(IRExpr::Block {
                statements: lift_block(&for_loop.body)?,
                expr: None,
            }),
        }),
        syn::Expr::Loop(loop_expr) => {
            let body = lift_block(&loop_expr.body)?;

            Ok(IRExpr::Loop {
                body: Box::new(IRExpr::Block {
                    statements: body,
                    expr: None,
                }),
            })
        }
        syn::Expr::Struct(struct_lit) => Ok(IRExpr::Struct {
            name: Box::new(IRExpr::Path(
                struct_lit
                    .path
                    .segments
                    .iter()
                    .map(|seg| lift_path_segment(seg))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            fields: struct_lit
                .fields
                .iter()
                .map(|field| Ok((lift_member(&field.member)?, lift_expr(&field.expr)?)))
                .collect::<Result<Vec<_>, _>>()?,
        }),
        syn::Expr::Reference(_) => panic!("References are not allowed."),
        _ => panic!("{:#?} not implemented", expr),
    }
}

/// Lifts a Syn `LocalInit` into an expression
pub fn lift_local_init(init: &LocalInit) -> DiagResult<IRExpr> {
    lift_expr(&init.expr)
}

/// Lifts a Syn `Local` into a statement
pub fn lift_local(stmt: Local) -> DiagResult<IRStmt> {
    let ident = lift_pat(&stmt.pat, None)?;
    let init = match &stmt.init {
        None => IRExpr::Null,
        Some(init) => lift_local_init(init)?,
    };

    Ok(IRStmt::Let {
        variable: ident,
        expr: init,
    })
}

/// Lifts a Syn `Visibility` into a string
pub fn lift_visibility(vis: &syn::Visibility) -> DiagResult<String> {
    match vis {
        syn::Visibility::Public(_) => Ok("public".into()),
        syn::Visibility::Inherited => Ok("inherited".into()),
        syn::Visibility::Restricted(_restriction) => {
            // TODO: implement restrictions
            Ok("restricted".into())
        }
    }
}

/// Lifts a Syn `ReturnType` into an IRType
pub fn lift_return_type(ty: &syn::ReturnType) -> DiagResult<IRType> {
    match ty {
        syn::ReturnType::Default => Ok(IRType::None),
        syn::ReturnType::Type(_, ty) => Ok(lift_ty(ty)?),
    }
}

/// Lifts a Syn `Block` into an array of statements
fn lift_block(block: &syn::Block) -> Result<Vec<IRStmt>, Diagnostic> {
    block.stmts.iter().map(|stmt| lift_stmt(stmt)).collect()
}

/// Lifts a Syn `Stmt` into a statement
pub fn lift_stmt(stmt: &syn::Stmt) -> DiagResult<IRStmt> {
    match stmt {
        syn::Stmt::Item(item) => lift_item(item),
        syn::Stmt::Local(local) => lift_local(local.clone()),
        syn::Stmt::Expr(expr, semi) => match semi {
            None => Ok(IRStmt::Expr(lift_expr(expr)?)),
            Some(_) => Ok(IRStmt::Semi(lift_expr(expr)?)),
        },
        _ => todo!(),
    }
}

/// Lifts a Syn `Item` into a statement
pub fn lift_item(item: &syn::Item) -> DiagResult<IRStmt> {
    match item {
        syn::Item::Fn(function) => {
            let visibility = lift_visibility(&function.vis)?;
            let name = function.sig.ident.to_string();

            let params: Vec<IRIdent> = function
                .sig
                .inputs
                .iter()
                .map(|param| -> Result<IRIdent, Diagnostic> {
                    match param {
                        syn::FnArg::Typed(arg) => lift_pat(&arg.pat, Some(&arg.ty)),
                        syn::FnArg::Receiver(re) => {
                            Err(diag("`self` parameters are not supported yet", re.span()))
                        }
                    }
                })
                .collect::<Result<_, _>>()?;

            Ok(IRStmt::FnDecl {
                name: name,
                parameters: params,
                return_type: lift_return_type(&function.sig.output)?,
                body: lift_block(&function.block)?
                    .into_iter()
                    .map(Box::new)
                    .collect::<Vec<_>>(),
                vis: visibility,
            })
        }

        // TODO: make const ACTUALLY const
        syn::Item::Const(r#const) => {
            let val = lift_expr(&r#const.expr);
            let ident = lift_ident(&r#const.ident);

            Ok(IRStmt::Let {
                variable: IRIdent::Identifier {
                    name: ident?,
                    mutable: false,
                    ty: Some(lift_ty(&r#const.ty)?),
                    init: None,
                },
                expr: val?,
            })
        }

        _ => todo!(),
    }
}

/// Converts a Syn tree into IR
pub fn lift_syn(file: &File) -> DiagResult<Vec<IRStmt>> {
    file.items
        .iter()
        .map(|item| lift_item(item))
        .collect::<Result<Vec<_>, _>>()
}
