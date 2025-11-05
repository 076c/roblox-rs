// Rust transformer

mod ast;
mod visitor;
use crate::ast::*;
use std::collections::HashMap;
use syn::{Expr, File, Token, parse::Parser, punctuated::Punctuated};

/// Parses a string into a syn `File`
pub fn parse_to_ast(source: String) -> Result<File, syn::Error> {
    syn::parse_str(&source.as_str())
}

/// Testing function
pub fn test() -> Vec<Result<LuauAst, syn::Error>> {
    let test1 = transform_source("const a: u8 = 1;".to_string());

    vec![Ok(test1)]
}

const NUMBER_INVALID: f64 = -0.0001;
const STRING_INVALID: &str = "_INVALID";

/// Transforms a syn `Lit` into a Luau expression
pub fn transform_lit(lit: &syn::Lit) -> Option<Box<dyn LuauExpression>> {
    match lit {
        syn::Lit::Bool(bool) => Some(Box::new(LuauBooleanLiteral::new(bool.value()))),
        syn::Lit::Char(char) => Some(Box::new(LuauStringLiteral::new(char.value().to_string()))),
        syn::Lit::Str(str) => Some(Box::new(LuauStringLiteral::new(str.value().to_string()))),
        syn::Lit::Int(int) => Some(Box::new(LuauNumberLiteral::new(
            int.base10_parse::<f64>().unwrap_or(NUMBER_INVALID),
        ))),
        syn::Lit::Float(float) => Some(Box::new(LuauNumberLiteral::new(
            float.base10_parse::<f64>().unwrap_or(NUMBER_INVALID),
        ))),
        syn::Lit::CStr(str) => Some(Box::new(LuauStringLiteral::new(
            str.value().to_str().unwrap_or(STRING_INVALID).to_string(),
        ))),
        syn::Lit::Byte(byte) => Some(Box::new(LuauNumberLiteral::new(byte.value().into()))),
        syn::Lit::ByteStr(str) => Some(Box::new(LuauTableConstructor::new(
            str.value()
                .iter()
                .map(|t| LuauTableField::ArrayValue(Box::new(LuauNumberLiteral::new(*t as f64))))
                .collect(),
        ))),
        syn::Lit::Verbatim(literal) => {
            let str = literal.to_string();

            let mut val: Option<Box<dyn LuauExpression>> = match str.as_str() {
                "true" => Some(Box::new(LuauBooleanLiteral::new(true))),
                "false" => Some(Box::new(LuauBooleanLiteral::new(false))),
                _ => None,
            };

            if val.is_none() {
                // TODO: add proper parsing to this
                val = Some(Box::new(LuauStringLiteral::new(str)));
            }

            return val;
        }
        _ => todo!(),
    }
}

// macro_rules! transform_macro {
//     ($expr: expr, $mut_map: expr) => {
//         let caller = transform_path(&$expr.mac.path);
//         let parser = Punctuated::<Expr, Token![,]>::parse_terminated;
//         let args = parser
//             .parse2($expr.mac.tokens.clone())
//             .unwrap_or_else(|_| panic!("invalid macro arguments"));
//         let transformed_args = args
//             .iter()
//             .map(|arg| transform_expression(arg, $mut_map).unwrap())
//             .collect::<Vec<_>>();

//         Some(Box::new(LuauFunctionCall::new(
//             caller,
//             None,
//             transformed_args,
//         )))
//     };
// }

macro_rules! transform_macro {
    ($expr: expr, $mut_map: expr) => {
        Box::new({
            let caller = transform_path(&$expr.mac.path);
            let parser = Punctuated::<Expr, Token![,]>::parse_terminated;
            let args = parser
                .parse2($expr.mac.tokens.clone())
                .unwrap_or_else(|_| panic!("invalid macro arguments"));
            let transformed_args = args
                .iter()
                .map(|arg| transform_expression(arg, $mut_map).unwrap())
                .collect::<Vec<_>>();

            LuauFunctionCall::new(caller, None, transformed_args)
        })
    };
}

/// Transforms a syn `Expr` into a Luau expression
pub fn transform_expression(
    expr: &syn::Expr,
    mut_map: &HashMap<String, bool>,
) -> Option<Box<dyn LuauExpression>> {
    match expr {
        syn::Expr::Lit(literal) => transform_lit(&literal.lit),
        syn::Expr::Array(array) => Some(Box::new(LuauTableConstructor::new(
            array
                .elems
                .iter()
                .map(|element| {
                    LuauTableField::ArrayValue(
                        // TODO: don't use nil expression, panic
                        transform_expression(element, mut_map)
                            .unwrap_or(Box::new(LuauNilLiteral::new())),
                    )
                })
                .collect(),
        ))),
        syn::Expr::Paren(expr) => Some(Box::new(LuauGroupedExpression::new(transform_expression(
            &expr.expr, mut_map,
        )?))),
        syn::Expr::Binary(binary_expr) => Some(Box::new(LuauBinaryExpression::new(
            transform_expression(&binary_expr.left, mut_map).unwrap(),
            transform_bin_op(binary_expr.op),
            transform_expression(&binary_expr.right, mut_map).unwrap(),
        ))),
        syn::Expr::Call(call_expr) => Some(Box::new(LuauFunctionCall::new(
            transform_expression(&call_expr.func, mut_map).unwrap(),
            None,
            call_expr
                .args
                .iter()
                .map(|arg| transform_expression(arg, mut_map).unwrap())
                .collect(),
        ))),
        syn::Expr::Closure(closure_expr) => {
            Some(Box::new(LuauFunctionLiteral::new(LuauFunctionBody::new(
                vec![],
                closure_expr
                    .inputs
                    .iter()
                    .map(|input| {
                        LuauBinding::new(
                            match input {
                                syn::Pat::Ident(i) => LuauVar::Name(i.ident.to_string()),
                                _ => panic!("invalid parameter"),
                            },
                            None,
                            match input {
                                syn::Pat::Ident(i) => i.mutability.is_some(),
                                _ => panic!("invalid parameter"),
                            },
                        )
                    })
                    .collect(),
                false,
                None,
                match &*closure_expr.body {
                    syn::Expr::Block(block) => transform_block(&block.block, mut_map),
                    _ => panic!("invalid expression"),
                },
            ))))
        }
        syn::Expr::Path(path) => match path.path.get_ident() {
            Some(ident) => Some(Box::new(LuauIdentifierLiteral::new(ident.to_string()))),
            None => panic!("invalid path"),
        },
        syn::Expr::Field(field_expr) => match &field_expr.member {
            syn::Member::Named(name) => Some(Box::new(LuauMemberExpression::new(
                transform_expression(&field_expr.base, mut_map).unwrap(),
                name.to_string(),
                None,
            ))),
            syn::Member::Unnamed(field) => Some(Box::new(LuauFieldExpression::new(
                transform_expression(&field_expr.base, mut_map).unwrap(),
                Box::new(LuauNumberLiteral::new(field.index as f64)),
                None,
            ))),
        },
        syn::Expr::Index(index_expr) => Some(Box::new(LuauFieldExpression::new(
            transform_expression(&index_expr.expr, mut_map).unwrap(),
            transform_expression(&index_expr.index, mut_map).unwrap(),
            None,
        ))),
        syn::Expr::Macro(macro_expr) => Some(transform_macro!(macro_expr, mut_map)),
        syn::Expr::MethodCall(method_expr) => Some(Box::new(LuauFunctionCall::new(
            transform_expression(&method_expr.receiver, mut_map).unwrap(),
            Some(method_expr.method.to_string()),
            method_expr
                .args
                .iter()
                .map(|arg| transform_expression(arg, mut_map).unwrap())
                .collect(),
        ))),
        syn::Expr::Tuple(tuple_expr) => Some(Box::new(LuauTupleExpression::new(
            tuple_expr
                .elems
                .iter()
                .map(|elem| transform_expression(elem, mut_map).unwrap())
                .collect(),
        ))),
        _ => panic!("unknown type: {:?}", expr),
    }
}

fn transform_path_arguments(path_argument: &syn::PathArguments) -> Option<LuauTypedef> {
    match path_argument {
        syn::PathArguments::None => None,
        _ => todo!(),
    }
}

/// Transforms a syn `Path` into a Luau path
fn transform_path(path: &syn::Path) -> Box<dyn LuauExpression> {
    // TODO: compile in path arguments
    let mut base_path: Box<dyn LuauExpression> = Box::new(LuauIdentifierLiteral::new(
        path.segments[0].ident.to_string(),
    ));

    for segment in &path.segments {
        base_path = Box::new(LuauMemberExpression::new(
            base_path,
            segment.ident.to_string(),
            transform_path_arguments(&segment.arguments),
        ));
    }

    return base_path;
}

/// Transforms a syn `BinOp` into a Luau operator
pub fn transform_bin_op(op: syn::BinOp) -> String {
    match op {
        syn::BinOp::Add(_) => "+".to_string(),
        syn::BinOp::Sub(_) => "-".to_string(),
        syn::BinOp::Mul(_) => "*".to_string(),
        syn::BinOp::Div(_) => "/".to_string(),
        syn::BinOp::Rem(_) => "%".to_string(),
        syn::BinOp::AddAssign(_) => "+=".to_string(),
        syn::BinOp::SubAssign(_) => "-=".to_string(),
        syn::BinOp::MulAssign(_) => "*=".to_string(),
        syn::BinOp::DivAssign(_) => "/=".to_string(),
        syn::BinOp::RemAssign(_) => "%=".to_string(),
        syn::BinOp::And(_) => "and".to_string(),
        syn::BinOp::Or(_) => "or".to_string(),
        // TODO: bitwise operators
        _ => todo!(),
    }
}

/// Transforms a syn `Pat` into a Luau variable
pub fn transform_pat(pat: &syn::Pat) -> (LuauBinding, bool) {
    // transform_variable!(pat)
    match pat {
        syn::Pat::Ident(ident) => {
            let binding = LuauBinding::new(
                LuauVar::Name(ident.ident.to_string()),
                None,
                ident.mutability.is_some(),
            );
            (binding, ident.mutability.is_some())
        }
        // references are ignored
        syn::Pat::Reference(r#ref) => transform_pat(&r#ref.pat),
        syn::Pat::Tuple(tuple) => {
            let mut vars = vec![];
            for field in &tuple.elems {
                vars.push(transform_pat(field).0);
            }
            (LuauBinding::new(LuauVar::Tuple(vars), None, false), false)
        }
        syn::Pat::Type(type_pat) => transform_pat(&type_pat.pat),
        _ => todo!(),
    }
}

pub fn transform_statement(
    statement: &syn::Stmt,
    mut_map: &HashMap<String, bool>,
) -> Result<(Option<Vec<Box<dyn LuauStatement>>>, Vec<(String, bool)>), syn::Error> {
    match statement {
        syn::Stmt::Item(item) => transform_item(item, mut_map),
        syn::Stmt::Expr(expr, semicolon) => match semicolon {
            // Post processed afterwards
            Some(_) => Ok((
                Some(vec![Box::new(LuauExpressionStatement::new(
                    transform_expression(&expr, mut_map).unwrap(),
                ))]),
                vec![],
            )),
            _ => Ok((
                Some(vec![Box::new(LuauReturnStatement::new(vec![
                    transform_expression(&expr, mut_map).unwrap(),
                ]))]),
                vec![],
            )),
        },
        syn::Stmt::Macro(mac) => Ok((
            Some(vec![Box::new(LuauFunctionCallStatement::new(
                *transform_macro!(mac, mut_map),
            ))]),
            vec![],
        )),
        syn::Stmt::Local(local) => {
            let local_binding = transform_pat(&local.pat);
            let mut initializers = vec![];

            if local.init.is_some() {
                // TODO: ->diverge
                initializers.push(
                    transform_expression(&local.init.as_ref().unwrap().expr, mut_map)
                        .unwrap_or(Box::new(LuauNilLiteral::new())),
                );
            }

            Ok((
                Some(vec![Box::new(LuauAssignment::new(
                    vec![local_binding.0],
                    initializers,
                    Some("=".to_string()),
                ))]),
                vec![],
            ))
        }
    }
}

/// Transforms a syn `Block` into a Luau block
pub fn transform_block(block: &syn::Block, mut_map: &HashMap<String, bool>) -> LuauBlock {
    let mut statements: Vec<Box<dyn LuauStatement>> = vec![];
    let mut muts_written: Vec<(String, bool)> = vec![];
    let mut terminator: Option<Box<dyn LuauStatement>> = None;

    for stmt in &block.stmts {
        let (stmts, muts) = transform_statement(stmt, mut_map).unwrap();
        if let Some(stmts) = stmts {
            for stmt in stmts {
                if terminator.is_some() {
                    panic!("cannot execute after terminator")
                }

                match stmt.type_of() {
                    // Terminators (terminate control flow)
                    LuauStatementType::Return => terminator = Some(stmt),
                    LuauStatementType::Continue => terminator = Some(stmt),
                    LuauStatementType::Break => terminator = Some(stmt),

                    // Don't terminate control flow
                    _ => statements.push(stmt),
                };
            }
        }
        muts_written.extend(muts);
    }

    LuauBlock::new(statements, terminator)
}

/// Transforms a syn `Type` into a Luau type definition
pub fn transform_type() {}

/// Transforms a syn `FnArg` into a Luau function parameter and returns the mutability
pub fn transform_fn_arg(function_argument: &syn::FnArg) -> (LuauBinding, bool) {
    match function_argument {
        syn::FnArg::Receiver(reciever) => (
            LuauBinding::new(
                LuauVar::Name("self".to_string()),
                None,
                reciever.mutability.is_some(),
            ),
            reciever.mutability.is_some(),
        ),
        syn::FnArg::Typed(typed) => transform_pat(&typed.pat),
    }
}

/// Transforms a syn `Item` into a top-level Luau statement
/// and returns the statement and the new mutabilities written
pub fn transform_item(
    item: &syn::Item,
    mut_map: &HashMap<String, bool>,
) -> Result<(Option<Vec<Box<dyn LuauStatement>>>, Vec<(String, bool)>), syn::Error> {
    match item {
        syn::Item::Const(const_item) => {
            let name = const_item.ident.to_string();
            if mut_map.contains_key(&name) && *mut_map.get(&name).unwrap_or(&true) == false {
                // TODO: nested `let` values can overwrite outside `const` values. Fix this
                return Err(syn::Error::new(
                    const_item.ident.span(),
                    "mutating immutable variable",
                ));
            }
            let expr = transform_expression(&const_item.expr, mut_map);

            Ok((
                Some(vec![Box::new(LuauAssignment::new(
                    // TODO: don't use clone?
                    vec![LuauBinding::new(
                        LuauVar::Name(name.clone()),
                        None,
                        false, // const values are always immutable
                    )],
                    vec![expr.unwrap_or(Box::new(LuauNilLiteral::new()))],
                    Some("=".to_string()),
                ))]),
                vec![(name, false)],
            ))
        }

        syn::Item::Static(static_item) => {
            let name = static_item.ident.to_string();

            Ok((
                Some(vec![Box::new(LuauAssignment::new(
                    // TODO: don't use clone?
                    vec![LuauBinding::new(
                        LuauVar::Name(name.clone()),
                        None,
                        match static_item.mutability {
                            syn::StaticMutability::Mut(_) => true,
                            _ => false,
                        },
                    )],
                    vec![
                        transform_expression(&static_item.expr, mut_map)
                            .unwrap_or(Box::new(LuauNilLiteral::new())),
                    ],
                    Some("=".to_string()),
                ))]),
                vec![(name, false)],
            ))
        }

        syn::Item::Fn(function_item) => {
            let function_name = function_item.sig.ident.to_string();
            let parameters: Vec<_> = function_item
                .sig
                .inputs
                .iter()
                .map(transform_fn_arg)
                .collect();

            let bindings = parameters
                .iter()
                .map(|param| {
                    LuauBinding::new(
                        match &param.0.name {
                            LuauVar::Name(name) => LuauVar::Name(name.to_string()),
                            _ => panic!("invalid parameter"),
                        },
                        None,
                        param.1,
                    )
                })
                .collect();

            Ok((
                Some(vec![Box::new(LuauFunctionDeclaration::new(
                    // TODO: repeated use of clone :(
                    function_name.clone(),
                    vec![],
                    LuauFunctionBody::new(
                        vec![],
                        bindings,
                        false,
                        None,
                        transform_block(&function_item.block, mut_map),
                    ),
                ))]),
                vec![(function_name, false)],
            ))
        }
        syn::Item::Macro(mac) => match &mac.ident {
            Some(_ident) => Ok((None, vec![])),
            None => Ok((
                Some(vec![Box::new(LuauFunctionCallStatement::new(
                    *transform_macro!(mac, mut_map),
                ))]),
                vec![],
            )),
        },
        _ => todo!(),
    }
}

/// Transforms the syn `File` tree into a Luau AST without applying any modifications
pub fn transform(ast: &File) -> LuauAst {
    // Statements that get transformed
    let mut statements: Vec<Box<dyn LuauStatement>> = vec![];

    // Mutability map to store mutability of variables
    let mut mutability_map: HashMap<String, bool> = HashMap::new();

    // Last statement that terminates the block (main scope)
    let terminator: Option<Box<dyn LuauStatement>> = None;

    // Iterate every statement and add it to the list of statements
    ast.items.iter().for_each(|item| {
        let (created_statements, muts_written) = match transform_item(item, &mutability_map) {
            Ok((statements, muts_written)) => (statements, muts_written),
            Err(err) => {
                panic!("{}", err);
            }
        };

        if let Some(created_statements) = created_statements {
            for stmt in created_statements {
                statements.push(stmt);
            }
        }

        muts_written.iter().for_each(|m| {
            mutability_map.insert(m.0.clone(), m.1);
        });
    });

    LuauAst {
        main_block: LuauBlock {
            statements: statements,
            last_statement: terminator,
            mutability_map: mutability_map,
        },
    }
}

/// Transforms Rust source code into a Luau AST
pub fn transform_source(source: String) -> LuauAst {
    transform(&parse_to_ast(source).unwrap_or_else(|err| panic!("{}", err)))
}
