///! IR definition
///
/// Used for lifting
/// Part of roblox-rs

///! IR binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IRBinOp {
    // Arithmetic
    Plus,
    Minus,
    Mul,
    Div,
    Rem,
    // Logical
    LogAnd,
    LogOr,
    // Bitwise
    BitXor,
    BitOr,
    BitAnd,
    BitShl,
    BitShr,
    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

///! IR unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IRUnOp {
    Neg,   // -x
    Not,   // !x
    Deref, // *x
}

///! IR for a match clause
#[derive(Debug, Clone, PartialEq)]
pub struct MatchClause {
    /// Pattern to match (could be identifier, literal, tuple, etc.)
    pub pattern: IRPattern,

    /// Optional guard expression
    pub guard: Option<Box<IRExpr>>,

    /// Expression executed if matched
    pub body: Box<IRExpr>,
}

///! IR patterns
#[derive(Debug, Clone, PartialEq)]
pub enum IRPattern {
    /// Wildcard `_`
    Wild,

    /// Identifier `x`
    Ident(String),

    /// Literal match
    Literal(IRExpr),

    /// Tuple pattern `(a, b, ...)`
    Tuple(Vec<IRPattern>),

    /// Enum variant `Some(x)`
    EnumVariant {
        name: String,
        fields: Vec<IRPattern>,
    },
}

///! IR identifiers
/// Used in assigning
#[derive(Debug, Clone, PartialEq)]
pub enum IRIdent {
    /// Pattern `_` that matches any value
    Wild,

    /// Named identifier with optional attributes
    Identifier {
        /// Name of the identifier
        name: String,

        /// Whether the identifier is mutable (`let mut x`)
        mutable: bool,

        /// Optional type annotation (can be `None` if not specified)
        ty: Option<IRType>,

        /// Optional default / initializer expression
        init: Option<Box<IRExpr>>,
    },

    /// Tuple assignment
    Tuple {
        /// Identifiers in the tuple
        idents: Vec<IRIdent>,
    },
}

///! IR types definition
#[derive(Debug, Clone, PartialEq)]
pub enum IRType {
    /// Typed function/closure
    Function {
        /// Input arguments to the function
        input_args: Vec<(Option<String>, Box<IRType>)>,

        /// Return type
        return_type: Option<Box<IRType>>,
    },

    /// Type function (e.x Vec<String>)
    TypeFunction {
        /// Base caller
        caller: String,

        /// Input arguments
        args: Vec<Box<IRType>>,
    },

    /// Simple type definition
    SimpleType {
        /// Type
        base: String,
    },

    /// Array type
    Array {
        /// Type of the element inside the array
        element_type: Box<IRType>,

        /// Length of the array
        len: f64,
    },

    /// Member index
    Member {
        /// Base
        base: Box<IRType>,

        /// Member string
        member: String,
    },

    /// Parenthesized type
    Paren {
        /// Type in parenthesis
        ty: Box<IRType>,
    },

    /// Inferred type
    Infer,

    /// No type
    None,
}

///! IR expressions
#[derive(Debug, Clone, PartialEq)]
pub enum IRExpr {
    /// Literals
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Char(String),
    ByteChar(String),
    ByteString(Vec<u8>),
    CString(String),
    RawString(String),

    /// Identifier / variable reference
    Identifier(String),

    /// Member expression (`base.name`)
    Member {
        base: Box<IRExpr>,
        name: String,
    },

    /// Function call
    Call {
        callee: Box<IRExpr>,
        arguments: Vec<IRExpr>,
    },

    /// Method call
    MethodCall {
        callee: Box<IRExpr>,
        method: String,
        arguments: Vec<IRExpr>,
    },

    /// Binary operation
    BinaryOperation {
        left: Box<IRExpr>,
        operator: IRBinOp,
        right: Box<IRExpr>,
    },

    /// Unary operation
    UnaryOperation {
        operator: IRUnOp,
        expr: Box<IRExpr>,
    },

    /// If expression
    IfExpression {
        condition: Box<IRExpr>,
        then_clause: Box<IRExpr>,
        else_clause: Option<Box<IRExpr>>,
    },

    /// Loop expressions
    Loop {
        body: Box<IRExpr>,
    },

    While {
        condition: Box<IRExpr>,
        body: Box<IRExpr>,
    },

    ForLoop {
        iterator: Box<IRExpr>,
        variable: IRIdent,
        body: Box<IRExpr>,
    },

    /// Match expression
    Match {
        condition: Box<IRExpr>,
        cases: Vec<MatchClause>,
    },

    /// Tuple expression
    Tuple(Vec<IRExpr>),

    /// Array expression `[x, y, z]` or `[x; n]`
    Array(Vec<IRExpr>),

    /// Indexing expression `array[i]`
    Index {
        base: Box<IRExpr>,
        index: Box<IRExpr>,
    },

    /// Return expression
    Return(Option<Box<IRExpr>>),

    /// Yield expression
    Yield(Option<Box<IRExpr>>),

    /// Break
    Break(Option<Box<IRExpr>>),

    /// Continue
    Continue,

    /// Block expression `{ ... }`
    Block {
        statements: Vec<IRStmt>,
        expr: Option<Box<IRExpr>>,
    },

    /// Closure / lambda expression
    Closure {
        parameters: Vec<IRIdent>,
        body: Box<IRExpr>,
        is_move: bool,
        return_type: Option<IRType>,
    },

    /// Let expression `let x = expr`
    Let {
        variable: String,
        expr: Box<IRExpr>,
    },

    /// Path expression
    Path(Vec<String>),

    /// Parenthesized expression
    Paren(Box<IRExpr>),

    /// Await expression
    Await(Box<IRExpr>),

    /// Try expression
    Try(Box<IRExpr>),

    /// Cast expression
    Cast {
        ty: Box<IRType>,
        expr: Box<IRExpr>,
    },

    /// Struct expression
    Struct {
        /// Struct name
        name: Box<IRExpr>,

        /// Key-value field pairs
        fields: Vec<(String, IRExpr)>,
    },
}

///! IR statements
#[derive(Debug, Clone, PartialEq)]
pub enum IRStmt {
    Expr(IRExpr),
    Let {
        variable: IRIdent,
        expr: IRExpr,
    },
    Semi(IRExpr), // expression with semicolon

    /// Function declaration
    FnDecl {
        /// Function name
        name: String,

        /// Input parameters
        parameters: Vec<IRIdent>,

        /// Return type of the function
        return_type: IRType,

        /// Body of the function
        body: Vec<Box<IRStmt>>,

        /// Visibility
        vis: String,
    },
    // /// If statement
    // IfStatement {
    //     /// The if expression
    //     expr: IRExpr,
    // },
}
