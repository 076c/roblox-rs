///! Luau Abstrack Syntax Tree module
///
/// This module defines the abstract syntax tree used to represent Luau code.
///
///!
use std::collections::HashMap;
use std::fmt::Debug;

/// Represents a full Luau source file, also known as a `chunk`.
///
/// Grammar:
/// ```text
/// chunk ::= block
/// ```
#[derive(Debug)]
pub struct LuauAst {
    /// The main block of statements representing the program.
    pub main_block: LuauBlock,
}

impl LuauAst {
    pub fn new(main_block: LuauBlock) -> Self {
        Self { main_block }
    }
}

/// Represents a sequence of statements (a code block).
///
/// Grammar:
/// ```text
/// block ::= {stat [';']} [laststat [';']]
/// ```
#[derive(Debug)]
pub struct LuauBlock {
    /// The list of statements contained in this block.
    pub statements: Vec<Box<dyn LuauStatement>>,
    /// An optional terminating statement such as `return`, `break`, or `continue`.
    pub last_statement: Option<Box<dyn LuauLastStatement>>,
    /// Mutability map to check the mutability of variables in the block.
    pub mutability_map: HashMap<String, bool>,
}

impl LuauBlock {
    pub fn new(statements: Vec<Box<dyn LuauStatement>>) -> Self {
        Self {
            statements,
            last_statement: None,
            mutability_map: HashMap::new(),
        }
    }
}

/// Enumerates all types of statements recognized by the Luau grammar.
#[derive(Debug)]
pub enum LuauStatementType {
    Assignment,
    CompoundAssignment,
    FunctionCall,
    DoBlock,
    WhileLoop,
    RepeatLoop,
    IfStatement,
    NumericFor,
    GenericFor,
    FunctionDeclaration,
    LocalFunction,
    LocalBinding,
    TypeDefinition,
    FunctionTypeDefinition,
}

/// Enumerates all types of expressions recognized by the Luau grammar.
#[derive(Debug)]
pub enum LuauExpressionType {
    Nil,
    Boolean,
    Number,
    String,
    Identifier,
    Binary,
    Unary,
    FunctionCall,
    TableConstructor,
    FunctionLiteral,
    Grouped,
    IfElseExpression,
    StringInterpolation,
}

/// Represents any statement node.
pub trait LuauStatement: Debug {
    /// Returns the high-level classification of this statement.
    fn type_of(&self) -> LuauStatementType;
}

/// Represents any expression node.
pub trait LuauExpression: Debug {
    /// Returns the expression's classification.
    fn type_of(&self) -> LuauExpressionType;
}

/// Represents a `laststat` production, which terminates a block.
///
/// Grammar:
/// ```text
/// laststat ::= 'return' [explist] | 'break' | 'continue'
/// ```
#[derive(Debug)]
pub enum LuauLastStatementType {
    Return,
    Break,
    Continue,
}

/// Trait for terminal statements like `return`, `break`, or `continue`.
pub trait LuauLastStatement: Debug {
    fn type_of(&self) -> LuauLastStatementType;
}

/// Represents a Luau type definition reference (not a full type expression).
///
/// Used in bindings, function parameters, and return types.
#[derive(Debug)]
pub struct LuauTypedef {
    /// The base type name, e.g., `"number"`, `"string"`, `"MyType"`.
    pub base_type: String,
    /// Any generic type arguments, e.g., `T` in `Array<T>`.
    pub type_arguments: Vec<Box<dyn LuauExpression>>,
}

impl LuauTypedef {
    pub fn new(base_type: String, type_arguments: Vec<Box<dyn LuauExpression>>) -> Self {
        Self {
            base_type,
            type_arguments,
        }
    }
}

/// Represents a local variable binding, with an optional type annotation.
///
/// Grammar:
/// ```text
/// binding ::= NAME [':' Type]
/// ```
#[derive(Debug)]
pub struct LuauBinding {
    pub name: String,
    pub type_def: Option<LuauTypedef>,
    pub mutable: bool,
}

impl LuauBinding {
    pub fn new(name: String, type_def: Option<LuauTypedef>, mutable: bool) -> Self {
        Self {
            name,
            type_def,
            mutable,
        }
    }
}

/// Represents a variable reference or field access.
///
/// Grammar:
/// ```text
/// var ::= NAME | prefixexp '[' exp ']' | prefixexp '.' NAME
/// ```
#[derive(Debug)]
pub enum LuauVar {
    /// Simple variable name.
    Name(String),
    /// Table element access, e.g. `tbl[key]`.
    Index(Box<dyn LuauExpression>, Box<dyn LuauExpression>),
    /// Field access, e.g. `tbl.field`.
    Field(Box<dyn LuauExpression>, String),
}

/// Represents a variable assignment or compound assignment.
///
/// Grammar:
/// ```text
/// stat ::= varlist '=' explist | var compoundop exp
/// ```
#[derive(Debug)]
pub struct LuauAssignment {
    pub left: Vec<LuauVar>,
    pub right: Vec<Box<dyn LuauExpression>>,
    pub operator: Option<String>, // for +=, -=, etc.
}

impl LuauStatement for LuauAssignment {
    fn type_of(&self) -> LuauStatementType {
        if self.operator.is_some() {
            LuauStatementType::CompoundAssignment
        } else {
            LuauStatementType::Assignment
        }
    }
}

impl LuauAssignment {
    pub fn new(
        vars: Vec<LuauVar>,
        expressions: Vec<Box<dyn LuauExpression>>,
        operator: Option<String>,
    ) -> Self {
        Self {
            left: vars,
            right: expressions,
            operator: operator,
        }
    }

    pub fn new_compound(
        vars: Vec<LuauVar>,
        op: String,
        expression: Box<dyn LuauExpression>,
    ) -> Self {
        Self {
            left: vars,
            right: vec![expression],
            operator: Some(op),
        }
    }
}

/// Represents a function declaration.
///
/// Grammar:
/// ```text
/// stat ::= attributes 'function' funcname funcbody
/// ```
#[derive(Debug)]
pub struct LuauFunctionDeclaration {
    pub name: String,
    pub attributes: Vec<LuauAttribute>,
    pub body: LuauFunctionBody,
}

impl LuauStatement for LuauFunctionDeclaration {
    fn type_of(&self) -> LuauStatementType {
        LuauStatementType::FunctionDeclaration
    }
}

impl LuauFunctionDeclaration {
    pub fn new(name: String, attributes: Vec<LuauAttribute>, body: LuauFunctionBody) -> Self {
        Self {
            name,
            attributes,
            body,
        }
    }
}

/// Represents a function literal or declaration body.
///
/// Grammar:
/// ```text
/// funcbody ::= ['<' GenericTypeList '>'] '(' [parlist] ')' [':' ReturnType] block 'end'
/// ```
#[derive(Debug)]
pub struct LuauFunctionBody {
    pub generic_parameters: Vec<String>,
    pub parameters: Vec<LuauBinding>,
    pub variadic: bool,
    pub return_type: Option<LuauTypedef>,
    pub block: LuauBlock,
}

impl LuauFunctionBody {
    pub fn new(
        generic_parameters: Vec<String>,
        parameters: Vec<LuauBinding>,
        variadic: bool,
        return_type: Option<LuauTypedef>,
        block: LuauBlock,
    ) -> Self {
        Self {
            generic_parameters: generic_parameters,
            parameters: parameters,
            variadic: variadic,
            return_type: return_type,
            block: block,
        }
    }
}

/// Represents an attribute annotation, e.g., `@deprecated` or `@[foo(1)]`.
///
/// Grammar:
/// ```text
/// attribute ::= '@' NAME | '@[' parattr {',' parattr} ']'
/// ```
#[derive(Debug)]
pub struct LuauAttribute {
    pub name: String,
    pub parameters: Vec<Box<dyn LuauExpression>>,
}

/// Represents a `nil` literal.
///
/// Grammar:
/// ```text
/// literal ::= 'nil'
/// ```
#[derive(Debug)]
pub struct LuauNilLiteral;

impl LuauExpression for LuauNilLiteral {
    fn type_of(&self) -> LuauExpressionType {
        LuauExpressionType::Nil
    }
}

impl LuauNilLiteral {
    pub fn new() -> Self {
        Self
    }
}

/// Represents a boolean literal (`true` or `false`).
///
/// Grammar:
/// ```text
/// literal ::= 'true' | 'false'
/// ```
#[derive(Debug)]
pub struct LuauBooleanLiteral {
    pub value: bool,
}

impl LuauExpression for LuauBooleanLiteral {
    fn type_of(&self) -> LuauExpressionType {
        LuauExpressionType::Boolean
    }
}

impl LuauBooleanLiteral {
    pub fn new(value: bool) -> Self {
        Self { value }
    }
}

/// Represents a numeric literal.
///
/// Grammar:
/// ```text
/// literal ::= NUMBER
/// ```
#[derive(Debug)]
pub struct LuauNumberLiteral {
    pub value: f64,
}

impl LuauExpression for LuauNumberLiteral {
    fn type_of(&self) -> LuauExpressionType {
        LuauExpressionType::Number
    }
}

impl LuauNumberLiteral {
    pub fn new(value: f64) -> Self {
        Self { value }
    }
}

/// Represents a string literal.
///
/// Grammar:
/// ```text
/// literal ::= STRING
/// ```
#[derive(Debug)]
pub struct LuauStringLiteral {
    pub value: String,
}

impl LuauExpression for LuauStringLiteral {
    fn type_of(&self) -> LuauExpressionType {
        LuauExpressionType::String
    }
}

impl LuauStringLiteral {
    pub fn new(value: String) -> Self {
        Self { value }
    }
}

/// Represents a unary operator expression.
///
/// Grammar:
/// ```text
/// exp ::= unop exp
/// unop ::= '-' | 'not' | '#' | '~' | ...
/// ```
#[derive(Debug)]
pub struct LuauUnaryExpression {
    pub operator: String,
    pub operand: Box<dyn LuauExpression>,
}

impl LuauExpression for LuauUnaryExpression {
    fn type_of(&self) -> LuauExpressionType {
        LuauExpressionType::Unary
    }
}

impl LuauUnaryExpression {
    pub fn new(operator: String, operand: Box<dyn LuauExpression>) -> Self {
        Self { operator, operand }
    }
}

/// Represents a table constructor expression.
///
/// Grammar:
/// ```text
/// tableconstructor ::= '{' [fieldlist] '}'
/// fieldlist ::= field {fieldsep field} [fieldsep]
/// field ::= '[' exp ']' '=' exp | NAME '=' exp | exp
/// ```
#[derive(Debug)]
pub struct LuauTableConstructor {
    pub fields: Vec<LuauTableField>,
}

impl LuauExpression for LuauTableConstructor {
    fn type_of(&self) -> LuauExpressionType {
        LuauExpressionType::TableConstructor
    }
}

impl LuauTableConstructor {
    pub fn new(fields: Vec<LuauTableField>) -> Self {
        Self { fields }
    }
}

/// Represents a field in a table constructor.
#[derive(Debug)]
pub enum LuauTableField {
    /// `[exp] = exp`
    ExpressionKey(Box<dyn LuauExpression>, Box<dyn LuauExpression>),
    /// `name = exp`
    NamedKey(String, Box<dyn LuauExpression>),
    /// Just an expression value (array style)
    ArrayValue(Box<dyn LuauExpression>),
}

/// Represents a function literal.
///
/// Grammar:
/// ```text
/// function ::= 'function' funcbody
/// ```
#[derive(Debug)]
pub struct LuauFunctionLiteral {
    pub body: LuauFunctionBody,
}

impl LuauExpression for LuauFunctionLiteral {
    fn type_of(&self) -> LuauExpressionType {
        LuauExpressionType::FunctionLiteral
    }
}

impl LuauFunctionLiteral {
    pub fn new(body: LuauFunctionBody) -> Self {
        Self { body }
    }
}

/// Represents an inline `if-then-else` expression.
///
/// Grammar:
/// ```text
/// ifexp ::= 'if' exp 'then' exp 'else' exp
/// ```
#[derive(Debug)]
pub struct LuauIfElseExpression {
    pub condition: Box<dyn LuauExpression>,
    pub then_branch: Box<dyn LuauExpression>,
    pub else_branch: Box<dyn LuauExpression>,
}

impl LuauExpression for LuauIfElseExpression {
    fn type_of(&self) -> LuauExpressionType {
        LuauExpressionType::IfElseExpression
    }
}

impl LuauIfElseExpression {
    pub fn new(
        condition: Box<dyn LuauExpression>,
        then_branch: Box<dyn LuauExpression>,
        else_branch: Box<dyn LuauExpression>,
    ) -> Self {
        Self {
            condition,
            then_branch,
            else_branch,
        }
    }
}

/// Represents string interpolation (e.g. `"Hello, #{name}!"`).
///
/// Grammar:
/// ```text
/// stringinterp ::= STRING_WITH_EMBEDDED_EXPRESSIONS
/// ```
#[derive(Debug)]
pub struct LuauStringInterpolation {
    pub segments: Vec<LuauStringSegment>,
}

impl LuauExpression for LuauStringInterpolation {
    fn type_of(&self) -> LuauExpressionType {
        LuauExpressionType::StringInterpolation
    }
}

impl LuauStringInterpolation {
    pub fn new(segments: Vec<LuauStringSegment>) -> Self {
        Self { segments }
    }
}

/// Represents a segment of a string interpolation, either raw text or an expression.
#[derive(Debug)]
pub enum LuauStringSegment {
    Text(String),
    Expression(Box<dyn LuauExpression>),
}

/// Represents an identifier reference.
///
/// Grammar:
/// ```text
/// simpleexp ::= NAME
/// ```
#[derive(Debug)]
pub struct LuauIdentifierLiteral {
    pub name: String,
}

impl LuauExpression for LuauIdentifierLiteral {
    fn type_of(&self) -> LuauExpressionType {
        LuauExpressionType::Identifier
    }
}

impl LuauIdentifierLiteral {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

/// Represents a grouped expression.
///
/// Grammar:
/// ```text
/// prefixexp ::= '(' exp ')'
/// ```
#[derive(Debug)]
pub struct LuauGroupedExpression {
    pub expression: Box<dyn LuauExpression>,
}

impl LuauExpression for LuauGroupedExpression {
    fn type_of(&self) -> LuauExpressionType {
        self.expression.type_of()
    }
}

impl LuauGroupedExpression {
    pub fn new(expression: Box<dyn LuauExpression>) -> Self {
        Self { expression }
    }
}

/// Represents a binary operator expression.
///
/// Grammar:
/// ```text
/// exp ::= asexp { binop exp }
/// binop ::= '+' | '-' | '*' | '/' | ... | 'and' | 'or'
/// ```
#[derive(Debug)]
pub struct LuauBinaryExpression {
    pub left: Box<dyn LuauExpression>,
    pub operator: String,
    pub right: Box<dyn LuauExpression>,
}

impl LuauExpression for LuauBinaryExpression {
    fn type_of(&self) -> LuauExpressionType {
        LuauExpressionType::Binary
    }
}

impl LuauBinaryExpression {
    pub fn new(
        left: Box<dyn LuauExpression>,
        operator: String,
        right: Box<dyn LuauExpression>,
    ) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }
}

/// Represents a function call expression.
///
/// Grammar:
/// ```text
/// functioncall ::= prefixexp funcargs | prefixexp ':' NAME funcargs
/// ```
#[derive(Debug)]
pub struct LuauFunctionCall {
    pub callee: Box<dyn LuauExpression>,
    pub method_name: Option<String>,
    pub arguments: Vec<Box<dyn LuauExpression>>,
}

impl LuauExpression for LuauFunctionCall {
    fn type_of(&self) -> LuauExpressionType {
        LuauExpressionType::FunctionCall
    }
}

impl LuauFunctionCall {
    pub fn new(
        callee: Box<dyn LuauExpression>,
        method_name: Option<String>,
        arguments: Vec<Box<dyn LuauExpression>>,
    ) -> Self {
        Self {
            callee,
            method_name,
            arguments,
        }
    }
}
