use self::{chunk::Chunk, value::Value};
use crate::vm::opcode::OpCode;

pub mod chunk;
pub mod opcode;
pub mod value;

// #[derive(Debug, Clone, PartialEq)]
// pub struct RuntimeError(pub String);

// impl RuntimeError {
//     pub fn new(msg: String) -> RuntimeError {
//         RuntimeError(msg)
//     }
// }

// impl From<&str> for RuntimeError {
//     fn from(msg: &str) -> RuntimeError {
//         RuntimeError::new(msg.to_string())
//     }
// }

// pub type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug, Clone, PartialEq)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VM {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
}

impl VM {
    const STACK_MAX: usize = 256;

    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: vec![],
        }
    }

    pub fn run(&mut self) -> InterpretResult {
        loop {
            let instr = self.read_instr();
            log::trace!("Instr: {:?}", instr);
            log::trace!("Stack: {:?}", self.stack);
            match instr {
                OpCode::Const => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                OpCode::Add => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Value::Number(a), Value::Number(b)) => self.push(Value::Number(a + b)),
                        _ => return InterpretResult::RuntimeError,
                    }
                }
                OpCode::Sub => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Value::Number(a), Value::Number(b)) => self.push(Value::Number(a - b)),
                        _ => return InterpretResult::RuntimeError,
                    }
                }
                OpCode::Mul => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Value::Number(a), Value::Number(b)) => self.push(Value::Number(a * b)),
                        _ => return InterpretResult::RuntimeError,
                    }
                }
                OpCode::Div => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Value::Number(a), Value::Number(b)) => self.push(Value::Number(a / b)),
                        _ => return InterpretResult::RuntimeError,
                    }
                }
                OpCode::Neg => {
                    let value = self.pop();
                    match value {
                        Value::Number(n) => self.push(Value::Number(-n)),
                        _ => return InterpretResult::RuntimeError,
                    }
                }
                OpCode::Return => {
                    println!("{}", self.pop());
                    return InterpretResult::Ok;
                }
                _ => return InterpretResult::RuntimeError,
            }
        }
    }

    fn read_instr(&mut self) -> OpCode {
        let instr = self.chunk.code[self.ip];
        self.ip += 1;
        OpCode::from(instr)
    }

    fn read_constant(&mut self) -> Value {
        let op = self.read_instr();
        self.chunk.constants[op as usize]
    }

    fn push(&mut self, value: Value) {
        if self.stack.len() >= Self::STACK_MAX {
            panic!("Stack overflow");
        }
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack underflow")
    }
}
