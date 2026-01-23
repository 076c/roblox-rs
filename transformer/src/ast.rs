///!
/// Luau AST module
/// Part of roblox-rs
///
/// Follows the Luau grammar

/// Luau compound operators
pub enum LuauCompoundOp {
    Add,      // +=
    Sub,      // -=
    Mul,      // *=
    Div,      // /=
    FloorDiv, // //=
    Mod,      // %=
    Pow,      // ^=
    Concat,   // ..=
}

/// Luau binary operators
pub enum LuauBinOp {
    Add,      // +
    Sub,      // -
    Mul,      // *
    Div,      // /
    FloorDiv, // //
    Mod,      // %
    Pow,      // ^
    Concat,   // ..
    Eq,       // ==
    Ne,       // ~=
    Lt,       // <
    Le,       // <=
    Gt,       // >
    Ge,       // >=
    And,      // and
    Or,       // or
}

/// Luau unary operators
pub enum LuauUnaryOperators {
    Neg, // -
    Len, // #
    Not, // not
}

/// Luau statements
pub enum LuauStatement {
    /// Variable assignment (`foo = bar`)
    Assignment {
        variables: LuauBinding,
        values: Vec<LuauExpression>,
    },

    /// Compound assignment (`x += y`)
    CompoundAssignment {
        variable: LuauBinding,
        op: LuauCompoundOp,
        value: Vec<LuauExpression>,
    },

    /// Local variable assignment (`local x = y`)
    LocalAssignment {
        variables: LuauBinding,
        values: Vec<LuauExpression>,
    },

    /// Function call
    FunctionCall(LuauExpression),

    /// Do block
    DoBlock { body: Vec<LuauStatement> },

    /// While loop
    While {
        condition: LuauExpression,
        body: Vec<LuauStatement>,
    },

    /// Repeat-until loop
    RepeatUntil {
        body: Vec<LuauStatement>,
        condition: LuauExpression,
    },

    /// If statement (with optional elseif / else)
    If {
        condition: LuauExpression,
        then_body: Vec<LuauStatement>,
        elseif_clauses: Vec<(LuauExpression, Vec<LuauStatement>)>,
        else_body: Option<Vec<LuauStatement>>,
    },

    /// Numeric for loop (`for i = start, end [, step] do ... end`)
    ForNumeric {
        var: LuauBinding,
        start: LuauExpression,
        end: LuauExpression,
        step: Option<LuauExpression>,
        body: Vec<LuauStatement>,
    },

    /// Generic for loop (`for k, v in explist do ... end`)
    ForGeneric {
        vars: Vec<LuauBinding>,
        iter: Vec<LuauExpression>,
        body: Vec<LuauStatement>,
    },

    /// Function declaration
    FunctionDecl {
        name: Vec<String>, // e.g., ["Module", "submodule", "func"]
        parameters: Vec<LuauBinding>,
        vararg: Option<LuauBinding>, // optional '...'
        body: Vec<LuauStatement>,
        is_method: bool,               // colon-method function
        generics: Option<Vec<String>>, // Generic names
        return_type: Option<LuauType>,
        attributes: Vec<LuauAttribute>,
    },

    /// Local function declaration
    LocalFunctionDecl {
        name: LuauBinding,
        parameters: Vec<LuauBinding>,
        vararg: Option<LuauBinding>,
        body: Vec<LuauStatement>,
        generics: Option<Vec<String>>,
        return_type: Option<LuauType>,
        attributes: Vec<LuauAttribute>,
    },

    /// Type alias / export type
    TypeAlias {
        name: String,
        generics: Option<Vec<String>>,
        ty: LuauType,
        is_exported: bool,
    },

    /// Type function declaration
    TypeFunctionDecl {
        name: String,
        generics: Option<Vec<String>>,
        body: LuauType,
        is_exported: bool,
    },

    /// Return statement
    Return { values: Vec<LuauExpression> },

    /// Break statement
    Break,

    /// Continue statement
    Continue,
}

/// Luau expressions
pub enum LuauExpression {
    /// Nil literal
    Nil,

    /// Boolean literal
    Boolean(bool),

    /// Number literal
    Number(f64),

    /// String literal
    String(String),

    /// Varargs (...)
    Varargs,

    /// Identifier
    Identifier(String),

    /// Table constructor
    Table(Vec<LuauTableEntry>),

    /// Function call
    FunctionCall {
        callee: Box<LuauExpression>,
        arguments: Vec<LuauExpression>,
    },

    /// Member access (.)
    Member {
        base: Box<LuauExpression>,
        member: String,
    },

    /// Index access ([...])
    Index {
        base: Box<LuauExpression>,
        index: Box<LuauExpression>,
    },

    /// Binary operation
    BinaryOperation {
        left: Box<LuauExpression>,
        operator: LuauBinOp,
        right: Box<LuauExpression>,
    },

    /// Unary operation
    UnaryOperation {
        operator: LuauUnaryOperators,
        expr: Box<LuauExpression>,
    },

    /// Parenthesized expression
    Paren(Box<LuauExpression>),

    /// If-expression (inline)
    If {
        condition: Box<LuauExpression>,
        then_body: Vec<LuauStatement>,
        elseif_clauses: Vec<(LuauExpression, Vec<LuauStatement>)>,
        else_body: Option<Vec<LuauStatement>>,
    },

    /// Type-cast expression
    Typecast {
        expression: Box<LuauExpression>,
        ty: LuauType,
    },

    /// Closure / anonymous function
    FunctionLiteral {
        parameters: Vec<LuauBinding>,
        vararg: Option<LuauBinding>,
        body: Vec<LuauStatement>,
        generics: Option<Vec<String>>,
        return_type: Option<LuauType>,
        attributes: Vec<LuauAttribute>,
    },

    /// String interpolation (e.g., `"Hello ${name}"`)
    StringInterp(Vec<LuauExpression>),
}

/// Luau table entry
pub enum LuauTableEntry {
    Named {
        index: String,
        value: LuauExpression,
    },
    Unnamed(LuauExpression),
}

/// Luau binding (variable)
pub enum LuauBinding {
    Identifier {
        name: String,
        var_type: Option<LuauType>,
    },
    VarList(Vec<LuauBinding>),
}

/// Luau attribute (e.g., `@attr`, `@[attr(...)]`)
pub struct LuauAttribute {
    pub name: String,
    pub args: Vec<LuauExpression>,
}

/// Luau type
pub enum LuauType {
    Nil,
    Boolean,
    Number,
    String,
    Table(Box<LuauType>),
    Function {
        parameters: Vec<LuauType>,
        vararg: Option<Box<LuauType>>,
        return_type: Box<LuauType>,
    },
    Name(String),
    Union(Vec<LuauType>),
    Intersection(Vec<LuauType>),
    Optional(Box<LuauType>),
    Tuple(Vec<LuauType>),
    Variadic(Box<LuauType>),
    Generic(String),
}
