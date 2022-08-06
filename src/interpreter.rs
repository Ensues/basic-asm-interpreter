use std::collections::HashMap;
use std::fmt;
use std::collections;

pub struct AssemblerInterpreter {
}

const LABEL_CHAR: &u8 = &b':';
const COMMENT_CHAR: &u8 = &b';';
const NEWLINE_CHAR: &u8 = &b'\n';
const PARAMETER_SEPARATOR: &u8 = &b',';
const STRING_DELIMETER: &u8 = &b'\'';

pub enum Token {
    Int(i32),
    String(String),
    Identifier(String),
    Separator,
    LabelDef,
    NewLine
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Int(n) => f.write_fmt(format_args!("Int({})", n)),
            Token::String(s) => f.write_fmt(format_args!("String({})", s)),
            Token::Identifier(s) => f.write_fmt(format_args!("Identifier({})", s)),
            Token::Separator => f.write_str("Separator"),
            Token::LabelDef => f.write_str("LabelDef"),
            Token::NewLine => f.write_str("NewLine"),
        }
    }
}

pub struct Register<'a>(&'a str);
impl fmt::Debug for Register<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Register").field(&self.0).finish()
    }
}
impl Register<'_> {
    pub fn get_value(&self, mem: &HashMap<&str,i32>) -> Option<i32> {
        Some(*mem.get(self.0)?)
    }
}
pub enum NumValue<'a> {
    Int(i32),
    Register(&'a str),
}
impl fmt::Debug for NumValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(arg0) => f.debug_tuple("Int").field(arg0).finish(),
            Self::Register(arg0) => f.debug_tuple("Register").field(arg0).finish(),
        }
    }
}
impl NumValue<'_> {
    pub fn get_value(&self, mem: &HashMap<&str,i32>) -> Option<i32> {
        match &self {
            NumValue::Int(v) => Some(*v),
            NumValue::Register(r) => Some(*mem.get(r)?),
        }
    }
}
pub enum StringValue<'a> {
    Int(i32),
    Register(&'a str),
    String(String)
}
impl fmt::Debug for StringValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(arg0) => f.debug_tuple("Int").field(arg0).finish(),
            Self::Register(arg0) => f.debug_tuple("Register").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
        }
    }
}

pub enum Command<'a> {
    Mov(Register<'a>, NumValue<'a>),
    Add(Register<'a>, NumValue<'a>),
    Sub(Register<'a>, NumValue<'a>),
    Mul(Register<'a>, NumValue<'a>),
    Div(Register<'a>, NumValue<'a>),
    Label(&'a str),
    Cmp(NumValue<'a>, NumValue<'a>),
    Jmp(&'a str),
    Jne(&'a str),
    Je(&'a str),
    Jge(&'a str),
    Jg(&'a str),
    Jle(&'a str),
    Jl(&'a str),
    Call(&'a str),
    Ret,
    End,
    Msg(Vec<StringValue<'a>>)
}

impl fmt::Debug for Command<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mov(arg0, arg1) => f.debug_tuple("Mov").field(arg0).field(arg1).finish(),
            Self::Add(arg0, arg1) => f.debug_tuple("Add").field(arg0).field(arg1).finish(),
            Self::Sub(arg0, arg1) => f.debug_tuple("Sub").field(arg0).field(arg1).finish(),
            Self::Mul(arg0, arg1) => f.debug_tuple("Mul").field(arg0).field(arg1).finish(),
            Self::Div(arg0, arg1) => f.debug_tuple("Div").field(arg0).field(arg1).finish(),
            Self::Label(arg0) => f.debug_tuple("Label").field(arg0).finish(),
            Self::Cmp(arg0, arg1) => f.debug_tuple("Cmp").field(arg0).field(arg1).finish(),
            Self::Jmp(arg0) => f.debug_tuple("Jmp").field(arg0).finish(),
            Self::Jne(arg0) => f.debug_tuple("Jne").field(arg0).finish(),
            Self::Je(arg0) => f.debug_tuple("Je").field(arg0).finish(),
            Self::Jge(arg0) => f.debug_tuple("Jge").field(arg0).finish(),
            Self::Jg(arg0) => f.debug_tuple("Jg").field(arg0).finish(),
            Self::Jle(arg0) => f.debug_tuple("Jle").field(arg0).finish(),
            Self::Jl(arg0) => f.debug_tuple("Jl").field(arg0).finish(),
            Self::Call(arg0) => f.debug_tuple("Call").field(arg0).finish(),
            Self::Ret => write!(f, "Ret"),
            Self::End => write!(f, "End"),
            Self::Msg(arg0) => f.debug_tuple("Msg").field(arg0).finish(),
        }
    }
}

impl AssemblerInterpreter  {
    pub fn interpret(input: &str) -> Option<String> {
        let tokens = tokenize(input)?;
        let result = parse(&tokens);
        let (commands, label_indices) = match result {
            Some(val) => val,
            None => return None,
        };
        let mut memory = std::collections::HashMap::<&str,i32>::new();
        let mut cmp = (0i32, 0i32);
        let mut ip = 0usize;
        let mut call_stack = Vec::<usize>::new();
        let mut out = String::new();
        while ip < commands.len() {
            match &commands[ip] {
                Command::Mov(a, b) => { memory.insert(a.0, b.get_value(&memory)?); }
                Command::Add(a, b) => { memory.insert(a.0, a.get_value(&memory)? + b.get_value(&memory)?); }
                Command::Sub(a, b) => { memory.insert(a.0, a.get_value(&memory)? - b.get_value(&memory)?); }
                Command::Mul(a, b) => { memory.insert(a.0, a.get_value(&memory)? * b.get_value(&memory)?); }
                Command::Div(a, b) => { memory.insert(a.0, a.get_value(&memory)? / b.get_value(&memory)?); }
                Command::Cmp(a, b) => { cmp = (a.get_value(&memory)?, b.get_value(&memory)?); }
                Command::Jmp(a) => { ip = *label_indices.get(*a)?; }
                Command::Jne(a) => { if cmp.0 != cmp.1 { ip = *label_indices.get(*a)?; } },
                Command::Je(a) => { if cmp.0 == cmp.1 { ip = *label_indices.get(*a)?; } },
                Command::Jge(a) => { if cmp.0 >= cmp.1 { ip = *label_indices.get(*a)?; } },
                Command::Jg(a) => { if cmp.0 > cmp.1 { ip = *label_indices.get(*a)?; } },
                Command::Jle(a) => { if cmp.0 <= cmp.1 { ip = *label_indices.get(*a)?; } },
                Command::Jl(a) => { if cmp.0 < cmp.1 { ip = *label_indices.get(*a)?; } },
                Command::Call(a) => { 
                    call_stack.push(ip);
                    ip = *label_indices.get(*a)?; 
                },
                Command::Ret => {
                    ip = call_stack.pop()?
                },
                Command::End => return Some(out),
                Command::Msg(a) => {
                    for m in a.iter() {
                        match m {
                            StringValue::Int(i) => out.push_str(&i.to_string()),
                            StringValue::Register(r) => out.push_str(&memory.get(r)?.to_string()),
                            StringValue::String(s) => out.push_str(s),
                        }
                    }
                },
                _ => {}
            };
            ip+=1;
        }
        None
    }
}

pub fn parse<'a>(input: &'a Vec<Token>) -> Option<(Vec<Command<'a>>, collections::HashMap<&'a str,usize>)> {
    let mut label_indices = collections::HashMap::<&str,usize>::new();
    let mut commands = Vec::<Command>::new();
    let mut iter = input.iter().peekable();
    while let Some(i) = iter.next() {
        let ident_name = match i {
            Token::Int(_) => return None,
            Token::String(_) => return None,
            Token::Identifier(i) => i,
            Token::Separator => return None,
            Token::LabelDef => return None,
            _ => continue
        };
        match ident_name.as_str() {
            "mov" => {
                if let (Token::Identifier(reg), Token::Separator, val) = (iter.next()?, iter.next()?, iter.next()?) {
                    let set = match val {
                        Token::Int(i) => NumValue::Int(*i),
                        Token::Identifier(i) => NumValue::Register(i),
                        _ => return None,
                    };
                    commands.push(Command::Mov(Register(reg.as_str()), set));
                } else {
                    return None;
                }
            },
            t @ ("inc" | "dec") => {
                if let Token::Identifier(reg) = iter.next()? {
                    match t {
                        "inc" => commands.push(Command::Add(Register(reg.as_str()), NumValue::Int(1))),
                        "dec" => commands.push(Command::Sub(Register(reg.as_str()), NumValue::Int(1))),
                        _ => return None,
                    }
                    
                } else {
                    return None;
                }
            },
            t @ ("add" | "sub" | "mul" | "div") => {
                if let (Token::Identifier(reg), Token::Separator, val) = (iter.next()?, iter.next()?, iter.next()?) {
                    let set = match val {
                        Token::Int(i) => NumValue::Int(*i),
                        Token::Identifier(i) => NumValue::Register(i),
                        _ => return None,
                    };
                    match t {
                        "add" => commands.push(Command::Add(Register(reg.as_str()), set)),
                        "sub" => commands.push(Command::Sub(Register(reg.as_str()), set)),
                        "mul" => commands.push(Command::Mul(Register(reg.as_str()), set)),
                        "div" => commands.push(Command::Div(Register(reg.as_str()), set)),
                        _ => return None,
                    }
                } else {
                    return None;
                }
            },
            "cmp" => {
                if let (val1, Token::Separator, val2) = (iter.next()?, iter.next()?, iter.next()?) {
                    let set1 = match val1 {
                        Token::Int(i) => NumValue::Int(*i),
                        Token::Identifier(i) => NumValue::Register(i),
                        _ => return None,
                    };
                    let set2 = match val2 {
                        Token::Int(i) => NumValue::Int(*i),
                        Token::Identifier(i) => NumValue::Register(i),
                        _ => return None,
                    };
                    commands.push(Command::Cmp(set1, set2));
                } else {
                    return None;
                }
            },
            t @ ("jmp" | "je" | "jne" | "jge" | "jg" | "jl" | "jle" | "call") => {
                if let Token::Identifier(i) = iter.next()? {
                    match t {
                        "jmp" => commands.push(Command::Jmp(i)),
                        "je" => commands.push(Command::Je(i)),
                        "jne" => commands.push(Command::Jne(i)),
                        "jg" => commands.push(Command::Jg(i)),
                        "jge" => commands.push(Command::Jge(i)),
                        "jl" => commands.push(Command::Jl(i)),
                        "jle" => commands.push(Command::Jle(i)),
                        "call" => commands.push(Command::Call(i)),
                        _ => return None,
                    }
                } else {
                    return None;
                }
            },
            "ret" => {
                commands.push(Command::Ret)
            },
            "end" => {
                commands.push(Command::End)
            },
            "msg" => {
                let mut msg = Vec::<StringValue>::new();
                loop {
                    match iter.next()? {
                        Token::Identifier(i) => msg.push(StringValue::Register(i)),
                        Token::Int(i) => msg.push(StringValue::Int(*i)),
                        Token::String(i) => msg.push(StringValue::String(i.to_string())),
                        _ => return None,
                    }
                    if let Some(t) = iter.next() {
                        match t {
                            Token::NewLine => break,
                            Token::Separator => {},
                            _ => return None,
                        }
                    } else {
                        break
                    }
                }
                commands.push(Command::Msg(msg))
            },
            n @ _ => {
                if let Token::LabelDef = iter.peek()? {
                    if label_indices.contains_key(n) {
                        return None;
                    }
                    commands.push(Command::Label(n));
                    label_indices.insert(n, commands.len()-1 as usize);
                    iter.next();
                } else {
                    return None;
                }
            },
        }
    }
    Some((commands,label_indices))
}

// Takes in a reference to a string. This is the code input.
// Returns a list of Tokens, which will last the rest of the program unless coerced to a shorter lifetime.
pub fn tokenize(input: &str) -> Option<Vec<Token>> {
    let bytes = input.as_bytes();
    let mut out = Vec::new();
    let mut index = 0;
    while index < bytes.len() { // Loop over chars
        match &bytes[index] {
            n if is_valid_initial_identifier_char(n) => { // Identifier
                let mut identifier: String = String::new();
                while index < bytes.len() && is_valid_identifier_char(&bytes[index]) {
                    identifier.push(bytes[index] as char);
                    index+=1;
                }
                index -= 1;
                out.push(Token::Identifier(identifier))
            }
            n if n.is_ascii_digit() || n==&b'-' => { // Int
                let mut num: String = String::new();
                while index < bytes.len() && (bytes[index].is_ascii_digit() || bytes[index]==b'-') {
                    num.push(bytes[index] as char);
                    index+=1;
                }
                index -= 1;
                if let Ok(i) = num.parse::<i32>() {
                    out.push(Token::Int(i));
                } else {
                    return Option::None;
                };
            }
            STRING_DELIMETER => { // Strings
                let mut s: String = String::new();
                index+=1;
                while index < bytes.len() && bytes[index]!=*STRING_DELIMETER {
                    s.push(bytes[index] as char);
                    index+=1;
                }
                out.push(Token::String(s))
            }
            COMMENT_CHAR => { // Comment
                while index < bytes.len() && bytes[index]!=*NEWLINE_CHAR {
                    index+=1;
                }
                index -= 1;
            }
            NEWLINE_CHAR => out.push(Token::NewLine),
            LABEL_CHAR => out.push(Token::LabelDef),
            PARAMETER_SEPARATOR => out.push(Token::Separator),
            _ => {} // Ignore else
        };
        index += 1;
    };
    Some(out)
}

fn is_valid_identifier_char(c: &u8) -> bool {
    c.is_ascii_alphanumeric() || *c == b'_'
}
fn is_valid_initial_identifier_char(c: &u8) -> bool {
    c.is_ascii_alphabetic() || *c == b'_'
}