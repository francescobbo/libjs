pub struct Expr {
    // pub kind: ExprKind,

    // The following fields are used for binary expressions
    pub left: Box<Expr>,
    pub right: Box<Expr>,

    // The following fields are used for unary expressions
    pub expr: Box<Expr>,
} 