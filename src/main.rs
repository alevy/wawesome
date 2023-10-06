use std::{io::{BufReader, stdin, Read, BufRead}, error::Error};

trait Readable: Sized {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>>;
}

impl Readable for String {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<String, Box<dyn Error>> {
        let length = usize::read_from(input)?;
        let mut result = vec![0; length];
        input.read_exact(&mut result)?;
        String::from_utf8(result).map_err(Into::into)
    }
}

impl Readable for u8 {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
        let mut buf = [0];
        input.read_exact(&mut buf)?;
        Ok(buf[0])
    }
}

macro_rules! readable_unsigned_int {
    ($t:ty) => {
        impl Readable for $t {
            fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
                Ok(leb128::read::unsigned(input)? as Self)
            }
        }
    };
}

macro_rules! readable_signed_int {
    ($t:ty) => {
        impl Readable for $t {
            fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
                Ok(leb128::read::signed(input)? as Self)
            }
        }
    };
}

readable_unsigned_int!(usize);
readable_unsigned_int!(u32);
readable_signed_int!(i32);
readable_unsigned_int!(u64);
readable_signed_int!(i64);

impl Readable for f32 {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
        use byteorder::{ReadBytesExt, LittleEndian};
        Ok(input.read_f32::<LittleEndian>()?)
    }
}

impl Readable for f64 {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
        use byteorder::{ReadBytesExt, LittleEndian};
        Ok(input.read_f64::<LittleEndian>()?)
    }
}

impl<T: Readable> Readable for Vec<T> {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Vec<T>, Box<dyn Error>> {
        let mut result = Vec::new();
        let length = u32::read_from(input)?;
        for _ in 0..length {
            result.push(T::read_from(input)?);
        }
        Ok(result)
    }
}

#[derive(Debug)]
pub enum RefType {
    Funcref,
    Extern,
}

impl Readable for RefType {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<RefType, Box<dyn Error>> {
        let reftype = u8::read_from(input)?;
        match reftype {
            0x70 => Ok(RefType::Funcref),
            0x6F => Ok(RefType::Extern),
            _ => panic!("Incorrect reference type"),
        }
    }
}

#[derive(Debug)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
    V128,
    Funcref,
    Externref,
}

impl Readable for ValType {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<ValType, Box<dyn Error>> {
        let valtype = u8::read_from(input)?;
        match valtype {
            0x6F => Ok(ValType::Externref),
            0x70 => Ok(ValType::Funcref),
            0x7B => Ok(ValType::V128),
            0x7C => Ok(ValType::F64),
            0x7D => Ok(ValType::F32),
            0x7E => Ok(ValType::I64),
            0x7F => Ok(ValType::I32),
            s => panic!("Incorrect value type {:#x}", s),
        }
    }
}

#[derive(Debug)]
pub struct Limit {
    pub min: u32,
    pub max: Option<u32>,
}

impl Readable for Limit {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Limit, Box<dyn Error>> {
        let limitkind = u8::read_from(input)?;
        match limitkind {
            0 => {
                let min = u32::read_from(input)?;
                Ok(Limit { min, max: None })
            },
            1 => {
                let min = u32::read_from(input)?;
                let max = Some(u32::read_from(input)?);
                Ok(Limit { min, max })
            },
            _ => panic!("invalid limit"),
        }
    }
}

#[derive(Debug)]
pub struct Table {
    pub reftype: RefType,
    pub limits: Limit,
}

impl Readable for Table {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
        let reftype = RefType::read_from(input)?;
        let limits = Limit::read_from(input)?;
        Ok(Table {
            reftype,
            limits
        })
    }
}

#[derive(Debug)]
pub struct Memory {
    pub limits: Limit
}

impl Readable for Memory {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
        let limits = Limit::read_from(input)?;
        Ok(Memory {limits})
    }
}

#[derive(Debug)]
pub struct Memarg {
    pub align: u32,
    pub offset: u32,
}

impl Readable for Memarg {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
        Ok(Memarg {
            align: u32::read_from(input)?,
            offset: u32::read_from(input)?,
        })
    }
}

#[derive(Debug)]
pub struct Block {
    pub blocktype: i64, // TODO blocktype
    pub expr: Expression,
}

impl Readable for Block {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
        let blocktype = i64::read_from(input)?;
        let expr = Expression::read_from(input)?;
        Ok(Block { blocktype, expr })
    }
}

#[repr(u8)]
#[derive(Debug)]
pub enum Instruction {
    // Control Instructions
    Unreachable                          = 0x00,
    Nop                                  = 0x01,
    Block(Block)                         = 0x02,
    Loop(Block)                          = 0x03,
    If((Block, Option<Expression>))      = 0x04,
    Branch(Idx)                          = 0x0C,
    BranchIf(Idx)                        = 0x0D,
    BranchTable((Vec<Idx>, Idx))         = 0x0E,
    Return                               = 0x0F,
    Call(Idx)                            = 0x10,
    CallIndirect((Idx, Idx))             = 0x11,

    // Parametric Instructions
    Drop                                 = 0x1A,
    Select                               = 0x1B,
    SelectT(Vec<ValType>)                = 0x1C,

    // Variable Instructions
    LocalGet(Idx)                        = 0x20,
    LocalSet(Idx)                        = 0x21,
    LocalTee(Idx)                        = 0x22,
    GlobalGet(Idx)                       = 0x23,
    GlobalSet(Idx)                       = 0x24,

    // Memory Instructions
    Memarg(Memarg),
    MemorySize                           = 0x3F,
    MemoryGrow                           = 0x40,

    // Numeric Instructions
    ConstI32(i32)                        = 0x41,
    ConstI64(i64)                        = 0x42,
    ConstF32(f32)                        = 0x43,
    ConstF64(f64)                        = 0x44,
    Numeric(u8),  // TODO Placeholder for all numeric plain opcodes with no immediates
    Truncate(u8)                         = 0xFC,

    Unknown(u8),
}

#[derive(Debug)]
pub struct Expression {
    pub instructions: Vec<Instruction>,
    pub terminator: u8,
}

impl Readable for Expression {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
        let mut instructions = vec![];
        let mut opcode;
        loop {
            opcode = u8::read_from(input).expect("opcode");
            let instruction = match opcode {
                // Termination opcode
                _ if opcode == 0x0B || opcode == 0x05 => break,
                // Control Instructions
                0x00 => Instruction::Unreachable,
                0x01 => Instruction::Nop,
                0x02 => {
                    let block = Block::read_from(input)?;
                    Instruction::Block(block)
                },
                0x03 => {
                    let expr = Block::read_from(input)?;
                    Instruction::Loop(expr)
                },
                0x04 => {
                    let ifbranch = Block::read_from(input)?;
                    let elsebranch = if ifbranch.expr.terminator == 0x05 {
                        Some(Expression::read_from(input)?)
                    } else {
                        None
                    };
                    Instruction::If((ifbranch, elsebranch))
                },
                0x0C => Instruction::Branch(Idx::read_from(input)?),
                0x0D => Instruction::BranchIf(Idx::read_from(input)?),
                0x0E => Instruction::BranchTable((<Vec<Idx>>::read_from(input)?, Idx::read_from(input)?)),
                0x0F => Instruction::Return,
                0x10 => Instruction::Call(Idx::read_from(input)?),
                0x11 => Instruction::CallIndirect((Idx::read_from(input)?, Idx::read_from(input)?)),

                // Parametric Instructions
                0x1A => Instruction::Drop,
                0x1B => Instruction::Select,
                0x1C => Instruction::SelectT(<Vec<ValType>>::read_from(input)?),

                // Variable Instructions
                0x20 => Instruction::LocalGet(Idx::read_from(input)?),
                0x21 => Instruction::LocalSet(Idx::read_from(input)?),
                0x22 => Instruction::LocalTee(Idx::read_from(input)?),
                0x23 => Instruction::GlobalGet(Idx::read_from(input)?),
                0x24 => Instruction::GlobalSet(Idx::read_from(input)?),

                // Memory argument
                // TODO
                0x28..=0x3E => Instruction::Memarg(Memarg::read_from(input)?),
                0x3F => {
                    assert_eq!(u8::read_from(input)?, 0x0);
                    Instruction::MemorySize
                },
                0x40 => {
                    assert_eq!(u8::read_from(input)?, 0x0);
                    Instruction::MemoryGrow
                },

                // Numeric Instructions
                0x41 => Instruction::ConstI32(i32::read_from(input)?),
                0x42 => Instruction::ConstI64(i64::read_from(input)?),
                0x43 => Instruction::ConstF32(f32::read_from(input)?),
                0x44 => Instruction::ConstF64(f64::read_from(input)?),
                // TODO
                0x45..=0xC4 => Instruction::Numeric(opcode),
                0xFC => Instruction::Truncate(u8::read_from(input)?),

                unknown => panic!("Unknown opcode {:#x}", unknown),
            };
            instructions.push(instruction);
        }
        Ok(Expression { instructions, terminator: opcode })
    }
}

#[derive(Debug)]
pub struct Global {
    pub valtype: ValType,
    pub mutable: bool,
    pub expr: Expression,
}

impl Readable for Global {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
        let valtype = ValType::read_from(input)?;
        let ismut = u8::read_from(input)?;
        let expr = Expression::read_from(input)?;
        Ok(Global {
            valtype,
            mutable: ismut != 0,
            expr,
        })
    }
}

#[derive(Debug)]
pub struct Idx(u32);

impl Readable for Idx {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
        Ok(Idx(u32::read_from(input)?))
    }
}

#[derive(Debug)]
pub struct Export {
    pub name: String,
    pub desc: ExportDesc,
}

impl Readable for Export {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Export, Box<dyn Error>> {
        let name = String::read_from(input)?;
        let desc = ExportDesc::read_from(input)?;
        Ok(Export { name, desc })
    }
}

#[derive(Debug)]
pub struct Local {
    pub count: u32,
    pub valtype: ValType,
}

impl Readable for Local {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
        let count = u32::read_from(input)?;
        let valtype = ValType::read_from(input)?;
        Ok(Local { count, valtype })
    }
}

#[derive(Debug)]
pub struct Code {
    pub locals: Vec<Local>,
    pub body: Expression,
}

impl Readable for Code {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
        let code_length = usize::read_from(input)?;
        let mut buf: Vec<u8> = vec![0; code_length];
        input.read_exact(&mut buf)?;
        let mut code_block = BufReader::new(std::io::Cursor::new(buf));
        Ok(Code {
            locals: <Vec<Local>>::read_from(&mut code_block)?,
            body: Expression::read_from(&mut code_block)?,
        })
    }
}

#[derive(Debug)]
pub enum ExportDesc {
    Function(Idx),
    Table(Idx),
    Mem(Idx),
    Global(Idx),
    Code(Code),
}

impl Readable for ExportDesc {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<ExportDesc, Box<dyn Error>> {
        let desctype = u8::read_from(input)?;
        let idx = Idx::read_from(input)?;
        match desctype {
            0x0 => Ok(ExportDesc::Function(idx)),
            0x1 => Ok(ExportDesc::Table(idx)),
            0x2 => Ok(ExportDesc::Mem(idx)),
            0x3 => Ok(ExportDesc::Global(idx)),
            _ => panic!("Incorrect export descriptor {:#x}", desctype),
        }
    }
}


#[derive(Debug)]
pub struct Custom {
    pub name: String,
    pub data: Vec<u8>,
}

impl Readable for Custom {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
        let name = String::read_from(input)?;
        let mut data = Vec::new();
        input.read_to_end(&mut data)?;
        Ok(Custom { name, data })
    }
}

#[derive(Debug)]
pub struct Type {
    pub paramtypes: Vec<ValType>,
    pub returntypes: Vec<ValType>,
}

impl Readable for Type {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
        let sanity = u8::read_from(input)?;
        if sanity != 0x60 {
            panic!("Unexpected");
        }
        let paramtypes = <Vec<ValType>>::read_from(input)?;
        let returntypes = <Vec<ValType>>::read_from(input)?;
        Ok(Type { paramtypes, returntypes })
    }
}

#[derive(Debug)]
pub enum Section {
    Custom(Custom),
    Type(Vec<Type>),
    Function(Vec<Idx>),
    Table(Vec<Table>),
    Memory(Vec<Memory>),
    Global(Vec<Global>),
    Export(Vec<Export>),
    Code(Vec<Code>),
    Unknown(u8),
}

impl Readable for Section {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Section, Box<dyn Error>> {
        let section_id = u8::read_from(input)?;
        let section_len = usize::read_from(input)?;

        let mut buf: Vec<u8> = vec![0; section_len];
        input.read_exact(&mut buf)?;
        let mut section = BufReader::new(std::io::Cursor::new(buf));

        match section_id {
            0 => { // Custom
                Ok(Section::Custom(Custom::read_from(&mut section)?))
            },
            1 => { // Type section
                Ok(Section::Type(<Vec<Type>>::read_from(&mut section)?))

            },
            3 => { // Function section
                Ok(Section::Function(<Vec<Idx>>::read_from(&mut section)?))
            },
            4 => { // Table section
                Ok(Section::Table(<Vec<Table>>::read_from(&mut section)?))
            },
            5 => { // Memory section
                Ok(Section::Memory(<Vec<Memory>>::read_from(&mut section)?))
            },
            6 => { // Global section
                Ok(Section::Global(<Vec<Global>>::read_from(&mut section)?))
            },
            7 => { // Export section
                Ok(Section::Export(<Vec<Export>>::read_from(&mut section)?))
            }
            10 => { // Code section
                Ok(Section::Code(<Vec<Code>>::read_from(&mut section)?))
            }
            s => {
                Ok(Section::Unknown(s))
            }
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut input = BufReader::new(stdin());

    {
        let mut magic = [0; 4];
        input.read_exact(&mut magic)?;
        if magic != [0x0, 0x61, 0x73, 0x6d] {
            panic!("Not webassembly!")
        }
    }

    {
        let mut version = [0; 4];
        input.read_exact(&mut version)?;
        if version != [0x1, 0, 0, 0] {
            panic!("Not correct webassembly version!")
        }
    }

    while let Ok(section) = Section::read_from(&mut input) {
        println!("{:x?}", section);
    }

    Ok(())
}
