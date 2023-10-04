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

macro_rules! readable_number {
    ($t:ty) => {
        impl Readable for $t {
            fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
                Ok(leb128::read::unsigned(input)? as Self)
            }
        }
    };
}

readable_number!(u32);
readable_number!(i32);
readable_number!(i64);
readable_number!(f32);
readable_number!(f64);
readable_number!(usize);

impl<T: Readable> Readable for Vec<T> {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Vec<T>, Box<dyn Error>> {
        let mut result = Vec::new();
        let length = usize::read_from(input)?;
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

#[repr(u8)]
#[derive(Debug)]
pub enum Instruction {
    Unreachable                          = 0x00,
    Nop                                  = 0x01,
    Block(Expression)                    = 0x02,
    Loop(Expression)                     = 0x03,
    If((Expression, Option<Expression>)) = 0x04,
    Branch(Idx)                          = 0x0C,
    BranchIf(Idx)                        = 0x0D,
    BranchTable((Vec<Idx>, Idx))         = 0x0E,
    Return                               = 0x0F,
    Call(Idx)                            = 0x10,
    CallIndirecto((Idx, Idx))            = 0x11,


    ConstI32(i32)                        = 0x41,
    ConstI64(i64)                        = 0x42,
    ConstF32(f32)                        = 0x43,
    ConstF64(f64)                        = 0x44,

    Unknown(u8),
}

#[derive(Debug)]
pub struct Expression<const T: u8 = 0x0B> {
    pub instructions: Vec<Instruction>,
}

impl<const T: u8> Readable for Expression<T> {
    fn read_from<R: BufRead + Read>(input: &mut R) -> Result<Self, Box<dyn Error>> {
        let mut instructions = vec![];
        loop {
            let opcode = u8::read_from(input)?;
            match opcode {
                _ if opcode == T => break,
                0x41 => instructions.push(Instruction::ConstI32(i32::read_from(input)?)),
                0x42 => instructions.push(Instruction::ConstI64(i64::read_from(input)?)),
                0x43 => instructions.push(Instruction::ConstF32(f32::read_from(input)?)),
                0x44 => instructions.push(Instruction::ConstF64(f64::read_from(input)?)),
                unknown => instructions.push(Instruction::Unknown(unknown)),
            }
        }
        Ok(Expression { instructions })
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
