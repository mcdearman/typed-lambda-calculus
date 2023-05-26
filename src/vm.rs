use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeError(pub String);

impl RuntimeError {
    pub fn new(msg: String) -> RuntimeError {
        RuntimeError(msg)
    }
}

impl From<&str> for RuntimeError {
    fn from(msg: &str) -> RuntimeError {
        RuntimeError::new(msg.to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
    Load(u16),
    Store(Value),
    LoadConst(u16),
    StoreConst(Value),
    Push(Value),
    Pop,
    Dup,
    Swap,
    Drop,
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Not,
    Eq,
    Neq,
    Lt,
    Gt,
    Jump(u16),
    Jeq(u16),
    Jnz(u16),
    Call(u16),
    Return,
    Halt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Lambda(Rc<ObjClosure>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjClosure {
    pub fun: Rc<ObjFunction>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjFunction {
    pub arity: u16,
    pub code: Vec<Instr>,
    // pub name: String,
}

impl ObjFunction {
    pub fn new() -> Self {
        Self {
            arity: 0,
            code: vec![],
            // name: String::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallFrame {
    pub ip: usize,
    pub chunk: Vec<Instr>,
    pub bp: usize,
}

pub struct VM {
    stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
}

impl VM {
    pub fn new(code: Vec<Instr>) -> Self {
        Self {
            stack: vec![],
            call_stack: vec![],
        }
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("stack underflow")
    }

    pub fn exec(&mut self) -> Result<Value, RuntimeError> {
        todo!()
    }
}
