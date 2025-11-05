///! Luau Abstrack Syntax Tree module
///
/// This module defines the abstract syntax tree used to represent Luau code.
///
///!
#[allow(dead_code)]
use std::any::Any;
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
    pub last_statement: Option<Box<dyn LuauStatement>>,
    /// Mutability map to check the mutability of variables in the block.
    pub mutability_map: HashMap<String, bool>,
}

impl LuauBlock {
    pub fn new(
        statements: Vec<Box<dyn LuauStatement>>,
        last_statement: Option<Box<dyn LuauStatement>>,
    ) -> Self {
        Self {
            statements,
            last_statement: last_statement,
            mutability_map: HashMap::new(),
        }
    }
}

/// Enumerates all types of statements recognized by the Luau grammar.
#[derive(Debug, PartialEq)]
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
    Return,
    Break,
    Continue,

    // Not valid in Luau
    Expression,
}

/// Enumerates all types of expressions recognized by the Luau grammar.
#[derive(Debug, PartialEq)]
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
    Member,
    Field,
    Tuple,
}

/// Represents any statement node.
pub trait LuauStatement: Debug + Any {
    /// Returns the high-level classification of this statement.
    fn type_of(&self) -> LuauStatementType;
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

/// Represents any expression node.
pub trait LuauExpression: Debug + Any {
    /// Returns the expression's classification.
    fn type_of(&self) -> LuauExpressionType;
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

// Cause Rust to freak out, append to normal statements
// /// Represents a `laststat` production, which terminates a block.
// ///
// /// Grammar:
// /// ```text
// /// laststat ::= 'return' [explist] | 'break' | 'continue'
// /// ```
// #[derive(Debug)]
// pub enum LuauLastStatementType {
//     Return,
//     Break,
//     Continue,
// }

// /// Trait for terminal statements like `return`, `break`, or `continue`.
// pub trait LuauLastStatement: Debug + Any {
//     fn type_of(&self) -> LuauLastStatementType;
//     fn as_any(&self) -> &dyn Any;
//     fn as_any_mut(&mut self) -> &mut dyn Any;
// }

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
    pub name: LuauVar,
    pub type_def: Option<LuauTypedef>,
    pub mutable: bool,
}

impl LuauBinding {
    pub fn new(name: LuauVar, type_def: Option<LuauTypedef>, mutable: bool) -> Self {
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
    /// Tuple e.g `a, b, c`
    Tuple(Vec<LuauBinding>),
}

/// Represents a variable assignment or compound assignment.
///
/// Grammar:
/// ```text
/// stat ::= varlist '=' explist | var compoundop exp
/// ```
#[derive(Debug)]
pub struct LuauAssignment {
    pub left: Vec<LuauBinding>,
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauAssignment {
    pub fn new(
        vars: Vec<LuauBinding>,
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
        vars: Vec<LuauBinding>,
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

/// Represents a function call statement.
/// Grammar:
/// ```text
/// stat ::= functioncall
/// ```
#[derive(Debug)]
pub struct LuauFunctionCallStatement {
    pub call: LuauFunctionCall,
}

impl LuauStatement for LuauFunctionCallStatement {
    fn type_of(&self) -> LuauStatementType {
        LuauStatementType::FunctionCall
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauFunctionCallStatement {
    pub fn new(call: LuauFunctionCall) -> Self {
        Self { call }
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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

impl LuauAttribute {
    pub fn new(name: String, parameters: Vec<Box<dyn LuauExpression>>) -> Self {
        Self { name, parameters }
    }
}

/// Represents a type alias or type definition.
///
/// Grammar:
/// ```text
/// stat ::= ['export'] 'type' NAME ['<' GenericTypeListWithDefaults '>'] '=' Type
/// ```
#[derive(Debug)]
pub struct LuauTypeDefinition {
    pub name: String,
    pub generic_parameters: Vec<String>,
    pub type_expr: Box<dyn LuauExpression>,
    pub exported: bool,
}

impl LuauStatement for LuauTypeDefinition {
    fn type_of(&self) -> LuauStatementType {
        LuauStatementType::TypeDefinition
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauTypeDefinition {
    pub fn new(
        name: String,
        generic_parameters: Vec<String>,
        type_expr: Box<dyn LuauExpression>,
        exported: bool,
    ) -> Self {
        Self {
            name,
            generic_parameters,
            type_expr,
            exported,
        }
    }
}

/// Represents a `do ... end` block statement.
///
/// Grammar:
/// ```text
/// stat ::= 'do' block 'end'
/// ```
#[derive(Debug)]
pub struct LuauDoBlock {
    pub block: LuauBlock,
}

impl LuauStatement for LuauDoBlock {
    fn type_of(&self) -> LuauStatementType {
        LuauStatementType::DoBlock
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauDoBlock {
    pub fn new(block: LuauBlock) -> Self {
        Self { block }
    }
}

/// Represents a `while ... do ... end` loop.
///
/// Grammar:
/// ```text
/// stat ::= 'while' exp 'do' block 'end'
/// ```
#[derive(Debug)]
pub struct LuauWhileLoop {
    pub condition: Box<dyn LuauExpression>,
    pub body: LuauBlock,
}

impl LuauStatement for LuauWhileLoop {
    fn type_of(&self) -> LuauStatementType {
        LuauStatementType::WhileLoop
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauWhileLoop {
    pub fn new(condition: Box<dyn LuauExpression>, body: LuauBlock) -> Self {
        Self { condition, body }
    }
}

/// Represents a `repeat ... until ...` loop.
///
/// Grammar:
/// ```text
/// stat ::= 'repeat' block 'until' exp
/// ```
#[derive(Debug)]
pub struct LuauRepeatLoop {
    pub body: LuauBlock,
    pub condition: Box<dyn LuauExpression>,
}

impl LuauStatement for LuauRepeatLoop {
    fn type_of(&self) -> LuauStatementType {
        LuauStatementType::RepeatLoop
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauRepeatLoop {
    pub fn new(body: LuauBlock, condition: Box<dyn LuauExpression>) -> Self {
        Self { body, condition }
    }
}

/// Represents an `if ... then ... elseif ... else ... end` statement.
///
/// Grammar:
/// ```text
/// stat ::= 'if' exp 'then' block {'elseif' exp 'then' block} ['else' block] 'end'
/// ```
#[derive(Debug)]
pub struct LuauIfStatement {
    pub condition: Box<dyn LuauExpression>,
    pub then_block: LuauBlock,
    pub elseif_blocks: Vec<(Box<dyn LuauExpression>, LuauBlock)>,
    pub else_block: Option<LuauBlock>,
}

impl LuauStatement for LuauIfStatement {
    fn type_of(&self) -> LuauStatementType {
        LuauStatementType::IfStatement
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauIfStatement {
    pub fn new(
        condition: Box<dyn LuauExpression>,
        then_block: LuauBlock,
        elseif_blocks: Vec<(Box<dyn LuauExpression>, LuauBlock)>,
        else_block: Option<LuauBlock>,
    ) -> Self {
        Self {
            condition,
            then_block,
            elseif_blocks,
            else_block,
        }
    }
}

/// Represents a numeric `for` loop.
///
/// Grammar:
/// ```text
/// stat ::= 'for' binding '=' exp ',' exp [',' exp] 'do' block 'end'
/// ```
#[derive(Debug)]
pub struct LuauNumericFor {
    pub binding: LuauBinding,
    pub start: Box<dyn LuauExpression>,
    pub end: Box<dyn LuauExpression>,
    pub step: Option<Box<dyn LuauExpression>>,
    pub body: LuauBlock,
}

impl LuauStatement for LuauNumericFor {
    fn type_of(&self) -> LuauStatementType {
        LuauStatementType::NumericFor
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauNumericFor {
    pub fn new(
        binding: LuauBinding,
        start: Box<dyn LuauExpression>,
        end: Box<dyn LuauExpression>,
        step: Option<Box<dyn LuauExpression>>,
        body: LuauBlock,
    ) -> Self {
        Self {
            binding,
            start,
            end,
            step,
            body,
        }
    }
}

/// Represents a generic `for ... in ... do ... end` loop.
///
/// Grammar:
/// ```text
/// stat ::= 'for' bindinglist 'in' explist 'do' block 'end'
/// ```
#[derive(Debug)]
pub struct LuauGenericFor {
    pub bindings: Vec<LuauBinding>,
    pub iterators: Vec<Box<dyn LuauExpression>>,
    pub body: LuauBlock,
}

impl LuauStatement for LuauGenericFor {
    fn type_of(&self) -> LuauStatementType {
        LuauStatementType::GenericFor
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauGenericFor {
    pub fn new(
        bindings: Vec<LuauBinding>,
        iterators: Vec<Box<dyn LuauExpression>>,
        body: LuauBlock,
    ) -> Self {
        Self {
            bindings,
            iterators,
            body,
        }
    }
}

/// Represents a terminal statement like `return`, `break`, or `continue`.
#[derive(Debug)]
pub struct LuauReturnStatement {
    pub values: Vec<Box<dyn LuauExpression>>,
}

impl LuauStatement for LuauReturnStatement {
    fn type_of(&self) -> LuauStatementType {
        LuauStatementType::Return
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauReturnStatement {
    pub fn new(values: Vec<Box<dyn LuauExpression>>) -> Self {
        Self { values }
    }
}

#[derive(Debug)]
pub struct LuauBreakStatement;

impl LuauStatement for LuauBreakStatement {
    fn type_of(&self) -> LuauStatementType {
        LuauStatementType::Break
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauBreakStatement {
    pub fn new() -> Self {
        Self
    }
}

#[derive(Debug)]
pub struct LuauContinueStatement;

impl LuauStatement for LuauContinueStatement {
    fn type_of(&self) -> LuauStatementType {
        LuauStatementType::Continue
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauContinueStatement {
    pub fn new() -> Self {
        Self
    }
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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

/// Represents a member expression.
///
/// Grammar:
/// ```text
/// memberexp ::= prefixexp '.' NAME
/// ```
#[derive(Debug)]
pub struct LuauMemberExpression {
    pub object: Box<dyn LuauExpression>,
    pub member_name: String,
    pub generic_arguments: Option<LuauTypedef>,
}

impl LuauExpression for LuauMemberExpression {
    fn type_of(&self) -> LuauExpressionType {
        LuauExpressionType::Member
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauMemberExpression {
    pub fn new(
        object: Box<dyn LuauExpression>,
        member_name: String,
        generic_arguments: Option<LuauTypedef>,
    ) -> Self {
        Self {
            object,
            member_name,
            generic_arguments,
        }
    }
}

/// Represents a field expression.
///
/// Grammar:
/// ```text
/// field ::= '[' exp ']'
/// ```
#[derive(Debug)]
pub struct LuauFieldExpression {
    pub object: Box<dyn LuauExpression>,
    pub field_name: Box<dyn LuauExpression>,
    pub generic_arguments: Option<LuauTypedef>,
}

impl LuauExpression for LuauFieldExpression {
    fn type_of(&self) -> LuauExpressionType {
        LuauExpressionType::Field
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauFieldExpression {
    pub fn new(
        object: Box<dyn LuauExpression>,
        field_name: Box<dyn LuauExpression>,
        generic_arguments: Option<LuauTypedef>,
    ) -> Self {
        Self {
            object,
            field_name,
            generic_arguments,
        }
    }
}

// Statements that are not valid in vanilla Luau (post-processed afterwards)

/// Represents an expression statement.
/// Grammar: ```text
/// stmt ::= expr [';']
/// ```
#[derive(Debug)]
pub struct LuauExpressionStatement {
    pub expression: Box<dyn LuauExpression>,
}

impl LuauStatement for LuauExpressionStatement {
    fn type_of(&self) -> LuauStatementType {
        LuauStatementType::Expression
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauExpressionStatement {
    pub fn new(expression: Box<dyn LuauExpression>) -> Self {
        Self { expression }
    }
}

/// Tuple expression
/// Grammar: ```text
/// tuple ::= '(' exp { ',' exp } ')'
/// ```
#[derive(Debug)]
pub struct LuauTupleExpression {
    pub expressions: Vec<Box<dyn LuauExpression>>,
}

impl LuauExpression for LuauTupleExpression {
    fn type_of(&self) -> LuauExpressionType {
        LuauExpressionType::Tuple
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LuauTupleExpression {
    pub fn new(expressions: Vec<Box<dyn LuauExpression>>) -> Self {
        Self { expressions }
    }
}
