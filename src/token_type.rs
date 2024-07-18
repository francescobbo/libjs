#[derive(Debug, Clone)]
pub enum TokenType {
    LineTerminator,

    LeftParen, RightParen, LeftBrace, RightBrace, LeftBracket, RightBracket,
    Dot, TripleDot, Semicolon, Comma, Less, Greater, LessEqual, GreaterEqual,
    EqualEqual, NotEqual, TripleEqual, NotEqualEqual, Plus, Minus, Star, Modulo,
    StarStar, PlusPlus, MinusMinus, ShiftLeft, ShiftRight, UnsignedShiftRight,
    BinaryAnd, BinaryOr, BinaryXor, Bang, Tilde,
    And, Or, DoubleQuestion, Question, Colon, Equal,
    PlusEqual, MinusEqual, StarEqual, ModuloEqual, StarStarEqual,
    ShiftLeftEqual, ShiftRightEqual, UnsignedShiftRightEqual,
    BinaryAndEqual, BinaryOrEqual, BinaryXorEqual,
    AndEqual, OrEqual, DoubleQuestionEqual,
    Arrow, OptionalChaining,

    Div, DivEqual,

    Identifier, String, Number,
  
    // Keywords
    Await, Break, Case, Class, Const, Continue, Debugger, Default, Delete, Do,
    Else, Enum, Export, Extends, False, Finally, For, Function, If, Import, In,
    Instanceof, New, Null, Return, Super, Switch, This, Throw, True, Try,
    Typeof, Var, Void, While, With, Yield,
  
    EOF
}
