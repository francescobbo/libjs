lalrpop_mod!(tokens);

use std::collections::HashMap;

use lazy_static::lazy_static;
use crate::{error, token::{Literal, Token}, token_type::TokenType};

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut map = HashMap::new();
        map.insert("await", TokenType::Await);
        map.insert("break", TokenType::Break);
        map.insert("case", TokenType::Case);
        map.insert("class", TokenType::Class);
        map.insert("const", TokenType::Const);
        map.insert("continue", TokenType::Continue);
        map.insert("debugger", TokenType::Debugger);
        map.insert("default", TokenType::Default);
        map.insert("delete", TokenType::Delete);
        map.insert("do", TokenType::Do);
        map.insert("else", TokenType::Else);
        map.insert("enum", TokenType::Enum);
        map.insert("export", TokenType::Export);
        map.insert("extends", TokenType::Extends);
        map.insert("false", TokenType::False);
        map.insert("finally", TokenType::Finally);
        map.insert("for", TokenType::For);
        map.insert("function", TokenType::Function);
        map.insert("if", TokenType::If);
        map.insert("import", TokenType::Import);
        map.insert("in", TokenType::In);
        map.insert("instanceof", TokenType::Instanceof);
        map.insert("new", TokenType::New);
        map.insert("null", TokenType::Null);
        map.insert("return", TokenType::Return);
        map.insert("super", TokenType::Super);
        map.insert("switch", TokenType::Switch);
        map.insert("this", TokenType::This);
        map.insert("throw", TokenType::Throw);
        map.insert("true", TokenType::True);
        map.insert("try", TokenType::Try);
        map.insert("typeof", TokenType::Typeof);
        map.insert("var", TokenType::Var);
        map.insert("void", TokenType::Void);
        map.insert("while", TokenType::While);
        map.insert("with", TokenType::With);
        map.insert("yield", TokenType::Yield);
        map
    };
}

#[derive(PartialEq)]
enum ScannerGoal {
    InputElementDiv,
    InputElementRegExp,
    InputElementRegExpOrTemplateTail,
    InputElementTemplateTail,
    InputElementHashbangOrRegExp,
}

pub struct Scanner {
    source: String,
    goal: ScannerGoal,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
} 

impl Scanner {
    pub fn new(source: String) -> Scanner {
        Scanner {
            source,
            goal: ScannerGoal::InputElementDiv,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(TokenType::EOF, "".to_string(), Literal::Nil, self.line));
        self.tokens.clone()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) {
        let line_terminators = ['\n', '\r', '\u{2028}', '\u{2029}'];

        match self.peek() {
            '\t' | '\x0b' | '\x0c' | '\u{feff}' | ' ' | '\u{a0}' | '\u{1680}' | 
            ('\u{2000}'..='\u{200a}') | '\u{202f}' | '\u{205f}' | '\u{3000}' => {
                // White space code points are insignificant and are ignored.
                self.advance();
            },
            '\n' | '\u{2028}' | '\u{2029}' => {
                self.advance();
                self.add_basic_token(TokenType::LineTerminator);
            },
            '\r' => {
                self.advance();
                if self.peek() == '\n' {
                    self.advance();
                }
                self.add_basic_token(TokenType::LineTerminator);
            },
            '/' if self.peek_next() == '/' => {
                self.advance();
                self.advance();

                // Single line comment. Consume until a LineTerminator is found.
                while !line_terminators.contains(&self.peek()) && !self.is_at_end() {
                    self.advance();
                }
            },
            '/' if self.peek_next() == '*' => {
                self.advance();
                self.advance();

                let mut has_newline = false;
                // Multi line comment. Consume until a '*/' is found.
                while !(self.peek() == '*' && self.peek_next() == '/') && !self.is_at_end() {
                    if line_terminators.contains(&self.peek()) {
                        has_newline = true;
                    }
                    self.advance();
                }

                if self.is_at_end() {
                    error(self.line, "Unterminated multi-line comment.");
                    return;
                }

                self.advance();
                self.advance();

                /* if a MultiLineComment contains a line terminator code point,
                 * then the entire comment is considered to be a LineTerminator
                 * for purposes of parsing by the syntactic grammar. */
                if has_newline {
                    self.add_basic_token(TokenType::LineTerminator);
                }
            },
            '#' if self.peek_next() == '!' && self.goal == ScannerGoal::InputElementHashbangOrRegExp => {
                self.advance();
                self.advance();

                // Hashbang comment. Consume until a LineTerminator is found.
                while !line_terminators.contains(&self.peek()) && !self.is_at_end() {
                    self.advance();
                }
            },
            '{' => { self.advance(); self.add_basic_token(TokenType::LeftBrace) },
            '(' => { self.advance(); self.add_basic_token(TokenType::LeftParen) },
            ')' => { self.advance(); self.add_basic_token(TokenType::RightParen) },
            '[' => { self.advance(); self.add_basic_token(TokenType::LeftBracket) },
            ']' => { self.advance(); self.add_basic_token(TokenType::RightBracket) },
            '.' => {
                self.advance();
                if self.peek() == '.' && self.peek_next() == '.' {
                    self.advance();
                    self.advance();
                    self.add_basic_token(TokenType::TripleDot);
                } else {
                    // Careful here.
                    // Can be dot accessor or number literal.
                }
            },
            ';' => self.add_basic_token(TokenType::Semicolon),
            ',' => self.add_basic_token(TokenType::Comma),
            '<' if self.match_char('<') =>{
                if self.match_char('=') {
                    self.add_basic_token(TokenType::ShiftLeftEqual);
                } else {
                    self.add_basic_token(TokenType::ShiftLeft);
                }  
            },
            '<' => {
                let token_type = if self.match_char('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_basic_token(token_type);
            },
            '>' if self.match_char('>') => {
                if self.match_char('>') {
                    if self.match_char('=') {
                        self.add_basic_token(TokenType::UnsignedShiftRightEqual);
                    } else {
                        self.add_basic_token(TokenType::UnsignedShiftRight);
                    }
                } else {
                    if self.match_char('=') {
                        self.add_basic_token(TokenType::ShiftRightEqual);
                    } else {
                        self.add_basic_token(TokenType::ShiftRight);
                    }
                }
            },
            '>' => {
                let token_type = if self.match_char('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_basic_token(token_type);
            },
            '=' => {
                let token_type = if self.match_char('=') {
                    if self.match_char('=') {
                        TokenType::TripleEqual
                    } else {
                        TokenType::EqualEqual
                    }
                } else if self.match_char('>') {
                    TokenType::Arrow
                } else {
                    TokenType::Equal
                };
                self.add_basic_token(token_type);
            },
            '!' => {
                let token_type = if self.match_char('=') {
                    if self.match_char('=') {
                        TokenType::NotEqualEqual
                    } else {
                        TokenType::NotEqual
                    }
                } else {
                    TokenType::Bang
                };
                self.add_basic_token(token_type);
            },
            '+' => {
                let token_type = if self.match_char('+') {
                    TokenType::PlusPlus
                } else if self.match_char('=') {
                    TokenType::PlusEqual
                } else {
                    TokenType::Plus
                };
                self.add_basic_token(token_type);
            },
            '-' => {
                let token_type = if self.match_char('-') {
                    TokenType::MinusMinus
                } else if self.match_char('=') {
                    TokenType::MinusEqual
                } else {
                    TokenType::Minus
                };
                self.add_basic_token(token_type);
            },
            '*' => {
                if self.match_char('*') {
                    if self.match_char('=') {
                        self.add_basic_token(TokenType::StarStarEqual);
                    } else {
                        self.add_basic_token(TokenType::StarStar);
                    }
                } else if self.match_char('=') {
                    self.add_basic_token(TokenType::StarEqual);
                } else {
                    self.add_basic_token(TokenType::Star);
                }
            },
            '%' => {
                if self.match_char('=') {
                    self.add_basic_token(TokenType::ModuloEqual);
                } else {
                    self.add_basic_token(TokenType::Modulo);
                }
            },
            '&' => {
                if self.match_char('&') {
                    if self.match_char('=') {
                        self.add_basic_token(TokenType::AndEqual);
                    } else {
                        self.add_basic_token(TokenType::And);
                    }
                } else if self.match_char('=') {
                    self.add_basic_token(TokenType::BinaryAndEqual);
                } else {
                    self.add_basic_token(TokenType::BinaryAnd);
                }
            },
            '|' => {
                if self.match_char('|') {
                    if self.match_char('=') {
                        self.add_basic_token(TokenType::OrEqual);
                    } else {
                        self.add_basic_token(TokenType::Or);
                    }
                } else if self.match_char('=') {
                    self.add_basic_token(TokenType::BinaryOrEqual);
                } else {
                    self.add_basic_token(TokenType::BinaryOr);
                }
            },
            '^' => {
                if self.match_char('=') {
                    self.add_basic_token(TokenType::BinaryXorEqual);
                } else {
                    self.add_basic_token(TokenType::BinaryXor);
                }
            },
            '~' => self.add_basic_token(TokenType::Tilde),
            '?' => {
                let token_type = if self.match_char('?') {
                    if self.match_char('=') {
                        TokenType::DoubleQuestionEqual
                    } else {
                        TokenType::DoubleQuestion
                    }
                } else {
                    TokenType::Question
                };
                self.add_basic_token(token_type);
            },
            ':' => self.add_basic_token(TokenType::Colon),
            '"' => {
                self.string('"');
            },
            '\'' => {
                self.string('\'');
            },
            _ => {
                // Attempt with an identifier.
                let id_result = self.identifier_name(c);
                if id_result.is_err() {
                    error(self.line, "Unexpected character.");
                }

                if let Ok(Some(id)) = id_result {
                    if let Some(keyword) = KEYWORDS.get(id.as_str()) {
                        // This is a keyword.
                        self.add_basic_token(keyword.clone());
                    } else {
                        // This is an identifier.
                        self.add_token(TokenType::Identifier, Literal::String(id));
                    }
                } else {
                    let number_result = self.number(c);
                }
            }
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source.chars().nth(self.current - 1).unwrap()
    }

    fn add_token(&mut self, token_type: TokenType, literal: Literal) {
        let text = self.source.chars().skip(self.start).take(self.current - self.start).collect();
        self.tokens.push(Token::new(token_type, text, literal, self.line));
    }

    fn add_basic_token(&mut self, token_type: TokenType) {
        self.add_token(token_type, Literal::Nil);
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source.chars().nth(self.current).unwrap() != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source.chars().nth(self.current).unwrap()
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        self.source.chars().nth(self.current + 1).unwrap()
    }

    fn string(&mut self, quote: char) {
        let mut value = String::new();
        while !self.is_at_end() {
            let c = self.advance();
            
            if c == quote {
                break;
            }

            if self.is_line_terminator(c) {
                error(self.line, "Unterminated string.");
            }
            
            if c == '\\' {
                // This may be an escape sequence or a line continuation.
                if self.is_line_terminator(self.peek()) {
                    self.consume_line_terminator_sequence();
                    self.line += 1;
                } else {
                    let escape = self.advance();
                    match escape {
                        '\'' => value.push('\''),
                        '"' => value.push('"'),
                        'b' => value.push('\u{0008}'),
                        'f' => value.push('\u{000c}'),
                        'n' => value.push('\n'),
                        'r' => value.push('\r'),
                        't' => value.push('\t'),
                        'v' => value.push('\u{000b}'),
                        '0' => {
                            if self.peek().is_digit(10) {
                                // Octal escape sequence.
                            } else {
                                // Null character.
                                value.push('\0');
                            }
                        },
                        'x' => {
                            let ch1 = self.advance();
                            let ch2 = self.advance();

                            let hex = format!("{}{}", ch1, ch2);
                            value.push(char::from_u32(u32::from_str_radix(hex.as_str(), 16).unwrap()).unwrap());
                        },
                        'u' => {
                            self.match_char('{');

                            let mut hex = String::new();
                            hex.push(self.advance());
                            hex.push(self.advance());
                            hex.push(self.advance());
                            hex.push(self.advance());
                            if self.peek().is_ascii_hexdigit() {
                                hex.push(self.advance());
                            }

                            self.match_char('}');

                            value.push(char::from_u32(u32::from_str_radix(hex.as_str(), 16).unwrap()).unwrap());
                        },
                        _ => value.push(escape),
                    }
                }

            } else {
                value.push(c);
            }
        }
            
        self.add_token(TokenType::String, Literal::String(value));
    }

    fn is_line_terminator(&self, c: char) -> bool {
        c == '\n' || c == '\r' || c == '\u{2028}' || c == '\u{2029}'
    }

    fn consume_line_terminator_sequence(&mut self) {
        let c = self.advance();
        if c == '\r' && self.peek() == '\n' {
            self.advance();
        }
    }

    fn number(&mut self) -> Option<String> {
        let mut value = String::new();
        
        loop {
            let c = self.peek();

            // An "n" at the end of an integral literal indicates that it is a BigInt.
            if c == 'n' {
                if value.len() == 0 {
                    // More likely to be an identifier.
                    return None;
                }

                // Consume the 'n' and produce a BigInt token.
                self.advance();
                self.add_token(TokenType::Number, Literal::BigInt(value));
                return;
            }

            match c {
                '0'..='9' => {
                    self.advance();
                },
                '.' => {
                    self.advance();
                    self.decimal_digits();
                },
                'e' | 'E' => {
                    self.advance();
                    self.exponent_part();
                },
                _ => break,
            }
        }
    }

    fn identifier_name(&mut self, first: char) -> Result<Option<String>, SyntaxError> {
        let mut value = String::new();
        let mut ch: char = first;

        loop {
            if ch == '\\' {
                // Unicode escape sequence.
                ch = self.unicode_escape_sequence()?;
            }

            if value.len() == 0 {
                if unicode_id_start::is_id_start(ch) || ch == '$' || ch == '_' {
                    value.push(ch);
                } else {
                    // This is not an identifier at all, return None and let the lexer try other token types.
                    return Ok(None);
                }
            } else {
                if unicode_id_start::is_id_continue(ch) || ch == '$' {
                    value.push(ch);
                } else {
                    return Ok(Some(value));
                }
            }

            ch = self.advance();
        }
    }

    fn unicode_escape_sequence(&mut self) -> Result<char, SyntaxError> {
        if !self.match_char('u') {
            return Err(SyntaxError {
                line: self.line,
                message: "invalid escape sequence".to_string(),
            });
        }

        let mut hex = String::new();

        if self.match_char('{') {
            // Any number of hex digits.
            while self.peek().is_ascii_hexdigit() {
                hex.push(self.advance());
            }

            if !self.match_char('}') {
                return Err(SyntaxError {
                    line: self.line,
                    message: "invalid escape sequence".to_string(),
                });
            }
        } else {
            // Exactly four hex digits.
            hex.push(self.advance());
            hex.push(self.advance());
            hex.push(self.advance());
            hex.push(self.advance());
        }

        if hex.chars().any(|c| !c.is_ascii_hexdigit()) {
            return Err(SyntaxError {
                line: self.line,
                message: "invalid escape sequence".to_string(),
            });
        }

        let value = u32::from_str_radix(hex.as_str(), 16).unwrap();
        if value > 0x10FFFF {
            return Err(SyntaxError {
                line: self.line,
                message: "invalid escape sequence".to_string(),
            });
        }

        Ok(char::from_u32(value).unwrap())
    }
}

struct SyntaxError {
    line: usize,
    message: String,
}