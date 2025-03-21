use std::fmt::Display;

#[derive(Debug)]
pub enum Instruction {
    Inc(i64),
    Dec(i64),
    Right(usize),
    Left(usize),
    PutChar,
    PutInt,
    Input,
    BeginLoop(Option<usize>),
    EndLoop(Option<usize>),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Inc(n) => {
                if *n == 1 {
                    write!(f, "+")
                } else {
                    write!(f, "+{}", n)
                }
            }
            Instruction::Dec(n) => {
                if *n == 1 {
                    write!(f, "-")
                } else {
                    write!(f, "-{}", n)
                }
            }
            Instruction::Right(n) => {
                if *n == 1 {
                    write!(f, ">")
                } else {
                    write!(f, ">{}", n)
                }
            }
            Instruction::Left(n) => {
                if *n == 1 {
                    write!(f, "<")
                } else {
                    write!(f, "<{}", n)
                }
            }
            Instruction::PutChar => write!(f, "."),
            Instruction::PutInt => write!(f, "?"),
            Instruction::Input => write!(f, ","),
            Instruction::BeginLoop(_) => write!(f, "["),
            Instruction::EndLoop(_) => write!(f, "]"),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub enum CellSize {
    #[default]
    I8,
    I16,
    I32,
    I64,
}

#[derive(Debug)]
pub enum CrashReason {
    PointerUnderflow,
    PointerOverflow,
}

#[derive(Debug)]
pub enum MachineType {
    Running(RunningMachine),
    Waiting(WaitingMachine),
    Halted(HaltedMachine),
    Crashed(HaltedMachine, CrashReason),
}

impl MachineType {
    pub fn clear_stdout(&mut self) -> Vec<char> {
        match self {
            MachineType::Running(r) => r.clear_stdout(),
            MachineType::Waiting(w) => w.clear_stdout(),
            MachineType::Halted(h) => h.clear_stdout(),
            MachineType::Crashed(h, _) => h.clear_stdout(),
        }
    }

    pub fn get_state(&self) -> &MachineState {
        match self {
            MachineType::Running(r) => &r.state,
            MachineType::Waiting(w) => &w.state,
            MachineType::Halted(h) => &h.state,
            MachineType::Crashed(h, _) => &h.state,
        }
    }
}

#[derive(Debug)]
pub struct MachineState {
    memory: Vec<i64>,
    pointer: usize,
    instructions: Vec<Instruction>,
    instructiuon_pointer: usize,
    stdout: Vec<char>,
    cell_size: CellSize,
}

impl MachineState {
    pub fn get_memory(&self) -> &[i64] {
        &self.memory
    }

    pub fn get_pointer(&self) -> &usize {
        &self.pointer
    }

    pub fn get_instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn get_instruction_pointer(&self) -> &usize {
        &self.instructiuon_pointer
    }

    pub fn get_stdout(&self) -> &Vec<char> {
        &self.stdout
    }

    pub fn clear_stdout(&mut self) -> Vec<char> {
        std::mem::take(&mut self.stdout)
    }

    fn get_mask(&self) -> i64 {
        match self.cell_size {
            CellSize::I8 => 0xff,
            CellSize::I16 => 0xffff,
            CellSize::I32 => 0xffffffff,
            CellSize::I64 => 0xffffffffffffffffu64 as i64,
        }
    }
}

#[derive(Debug)]
pub struct RunningMachine {
    state: Box<MachineState>,
}

impl RunningMachine {
    fn step_internal(mut self) -> MachineType {
        if self.state.instructiuon_pointer >= self.state.instructions.len() {
            return MachineType::Halted(HaltedMachine { state: self.state });
        }
        let instruction = &self.state.instructions[self.state.instructiuon_pointer];
        match instruction {
            Instruction::Inc(n) => {
                self.state.memory[self.state.pointer] =
                    self.state.memory[self.state.pointer].wrapping_add(*n) & self.state.get_mask();
                self.state.instructiuon_pointer += 1;
                MachineType::Running(RunningMachine { state: self.state })
            }
            Instruction::Dec(n) => {
                self.state.memory[self.state.pointer] =
                    self.state.memory[self.state.pointer].wrapping_sub(*n) & self.state.get_mask();
                self.state.instructiuon_pointer += 1;
                MachineType::Running(RunningMachine { state: self.state })
            }
            Instruction::Right(n) => {
                self.state.pointer += *n;
                self.state.instructiuon_pointer += 1;
                if self.state.pointer >= self.state.memory.len() {
                    MachineType::Crashed(
                        HaltedMachine { state: self.state },
                        CrashReason::PointerOverflow,
                    )
                } else {
                    MachineType::Running(RunningMachine { state: self.state })
                }
            }
            Instruction::Left(n) => {
                if *n > self.state.pointer {
                    return MachineType::Crashed(
                        HaltedMachine { state: self.state },
                        CrashReason::PointerUnderflow,
                    );
                }
                self.state.pointer = self.state.pointer - *n;
                self.state.instructiuon_pointer += 1;
                if self.state.pointer >= self.state.memory.len() {
                    MachineType::Crashed(
                        HaltedMachine { state: self.state },
                        CrashReason::PointerUnderflow,
                    )
                } else {
                    MachineType::Running(RunningMachine { state: self.state })
                }
            }
            Instruction::PutChar => {
                self.state
                    .stdout
                    .push(self.state.memory[self.state.pointer] as u8 as char);
                self.state.instructiuon_pointer += 1;
                MachineType::Running(RunningMachine { state: self.state })
            }
            Instruction::PutInt => {
                self.state.stdout.extend_from_slice(
                    &self.state.memory[self.state.pointer]
                        .to_string()
                        .chars()
                        .collect::<Vec<char>>(),
                );
                self.state.instructiuon_pointer += 1;
                MachineType::Running(RunningMachine { state: self.state })
            }
            Instruction::Input => MachineType::Waiting(WaitingMachine { state: self.state }),
            Instruction::BeginLoop(out_ptr) => {
                if self.state.memory[self.state.pointer] != 0 {
                    self.state.instructiuon_pointer += 1;
                    MachineType::Running(RunningMachine { state: self.state })
                } else {
                    self.state.instructiuon_pointer = out_ptr.unwrap();
                    MachineType::Running(RunningMachine { state: self.state })
                }
            }
            Instruction::EndLoop(in_ptr) => {
                if self.state.memory[self.state.pointer] != 0 {
                    self.state.instructiuon_pointer = in_ptr.unwrap();
                    MachineType::Running(RunningMachine { state: self.state })
                } else {
                    self.state.instructiuon_pointer += 1;
                    MachineType::Running(RunningMachine { state: self.state })
                }
            }
        }
    }

    pub fn step(self) -> MachineType {
        match self.step_internal() {
            MachineType::Running(r) => {
                if *r.get_state().get_instruction_pointer()
                    >= r.get_state().get_instructions().len()
                {
                    MachineType::Halted(HaltedMachine { state: r.state })
                } else {
                    MachineType::Running(r)
                }
            }
            other => other,
        }
    }

    pub fn get_state(&self) -> &MachineState {
        &self.state
    }

    pub fn clear_stdout(&mut self) -> Vec<char> {
        self.state.clear_stdout()
    }
}

#[derive(Debug)]
pub struct WaitingMachine {
    state: Box<MachineState>,
}

impl WaitingMachine {
    pub fn input(mut self, value: i64) -> RunningMachine {
        self.state.memory[self.state.pointer] = value & self.state.get_mask();
        self.state.instructiuon_pointer += 1;
        RunningMachine { state: self.state }
    }

    pub fn get_state(&self) -> &MachineState {
        &self.state
    }

    pub fn clear_stdout(&mut self) -> Vec<char> {
        self.state.clear_stdout()
    }
}

#[derive(Debug)]
pub struct HaltedMachine {
    state: Box<MachineState>,
}

impl HaltedMachine {
    pub fn get_state(&self) -> &MachineState {
        &self.state
    }

    pub fn clear_stdout(&mut self) -> Vec<char> {
        self.state.clear_stdout()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CompileError {
    UnknownError,
    UnbalancedBrackets(usize),
}

fn gen_instructions(input: &str) -> Vec<Instruction> {
    let chars = input.chars().collect::<Vec<char>>();
    let mut instructions = Vec::new();
    let mut i = 0;

    macro_rules! repeatable_instruction {
        ($instruction: ident, $def_amount: expr, $param_type: ty) => {{
            i += 1;
            let mut amount = Vec::new();
            while i < chars.len() && chars[i].is_numeric() {
                amount.push(chars[i]);
                i += 1;
            }
            let amount = if amount.is_empty() {
                $def_amount
            } else {
                amount
                    .iter()
                    .collect::<String>()
                    .parse::<$param_type>()
                    .unwrap()
            };
            match instructions.last() {
                Some(Instruction::$instruction(_)) => {
                    let last = instructions.last_mut().unwrap();
                    match last {
                        Instruction::$instruction(n) => *n += amount,
                        _ => unreachable!(),
                    }
                }
                _ => instructions.push(Instruction::$instruction(amount)),
            }
        }};
    }

    while i < chars.len() {
        match chars[i] {
            '+' => repeatable_instruction!(Inc, 1, i64),
            '-' => repeatable_instruction!(Dec, 1, i64),
            '>' => repeatable_instruction!(Right, 1, usize),
            '<' => repeatable_instruction!(Left, 1, usize),
            '?' => {
                i += 1;
                instructions.push(Instruction::PutInt);
            }
            '.' => {
                i += 1;
                instructions.push(Instruction::PutChar);
            }
            ',' => {
                i += 1;
                instructions.push(Instruction::Input);
            }
            '[' => {
                i += 1;
                instructions.push(Instruction::BeginLoop(None));
            }
            ']' => {
                i += 1;
                instructions.push(Instruction::EndLoop(None));
            }
            _ => i += 1,
        }
    }
    instructions
}

pub fn find_loops(instructions: &mut [Instruction]) -> Result<(), CompileError> {
    let mut stack = Vec::new();
    let mut update_stack = Vec::new();
    for (idx, instruction) in instructions.iter().enumerate() {
        match instruction {
            Instruction::BeginLoop(_) => {
                stack.push(idx);
            }
            Instruction::EndLoop(_) => {
                let start = stack.pop().ok_or(CompileError::UnbalancedBrackets(idx))?;
                update_stack.push((start, idx));
            }
            _ => {}
        }
    }
    if !stack.is_empty() {
        return Err(CompileError::UnbalancedBrackets(stack[0]));
    }
    for (start, end) in update_stack.iter() {
        instructions[*start] = Instruction::BeginLoop(Some(*end));
        instructions[*end] = Instruction::EndLoop(Some(*start));
    }
    Ok(())
}

pub fn compile_bf(
    input: &str,
    memory_size: usize,
    cell_size: CellSize,
) -> Result<RunningMachine, CompileError> {
    let mut instructions = gen_instructions(input);
    find_loops(&mut instructions)?;

    Ok(RunningMachine {
        state: Box::new(MachineState {
            instructions,
            memory: vec![0; memory_size],
            pointer: 0,
            instructiuon_pointer: 0,
            stdout: Vec::new(),
            cell_size,
        }),
    })
}

pub fn run_machine(
    mut machine: RunningMachine,
    max_steps: usize,
    input_iterator: &mut dyn Iterator<Item = i64>,
) -> MachineType {
    for _ in 0..max_steps {
        match machine.step() {
            MachineType::Running(r) => machine = r,
            MachineType::Waiting(w) => match input_iterator.next() {
                Some(value) => {
                    machine = w.input(value);
                }
                None => return MachineType::Waiting(w),
            },
            other => return other,
        }
    }
    MachineType::Running(machine)
}
