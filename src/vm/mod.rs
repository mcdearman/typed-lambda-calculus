pub mod chunk;
pub mod opcode;
pub mod value;

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

pub type Result<T> = std::result::Result<T, RuntimeError>;

// #[derive(Debug, Clone, PartialEq)]
// pub struct CallFrame {
//     pub ip: usize,
//     pub chunk: Chunk,
//     pub bp: usize,
// }

// impl CallFrame {
//     pub fn new(ip: usize, chunk: Chunk, bp: usize) -> Self {
//         Self { ip, chunk, bp }
//     }
// }

// pub struct VM {
//     stack: Vec<Value>,
//     call_stack: Vec<CallFrame>,
// }

// impl VM {
// pub fn new(code: Vec<Instr>) -> Self {
//     Self {
//         stack: vec![],
//         call_stack: vec![],
//     }
// }

// fn push(&mut self, val: Value) {
//     self.stack.push(val);
// }

// fn pop(&mut self) -> Value {
//     self.stack.pop().expect("stack underflow")
// }

// pub fn exec(&mut self) -> Result<Value> {
//     todo!()
// }
// }
