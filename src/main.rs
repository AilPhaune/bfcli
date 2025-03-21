use std::{
    collections::VecDeque,
    env,
    io::{self, stdout, Write},
    time::SystemTime,
};

use bflib::{compile_bf, CellSize, MachineType};
use crossterm::{
    cursor,
    event::{self, Event, KeyCode, KeyEventKind, KeyModifiers},
    execute,
    style::{Color, ResetColor, SetBackgroundColor, SetForegroundColor},
    terminal::{self, ClearType},
};

#[derive(Debug, Default)]
struct ProgramLaunchState {
    pub code: Option<String>,
    pub input: Option<String>,
    pub interactive: bool,
    pub memory_size: Option<usize>,
    pub cell_datatype: Option<CellSize>,
}

#[derive(Debug, Default)]
struct ResolvedLaunchState {
    pub code: String,
    pub input: Option<String>,
    pub interactive: bool,
    pub memory_size: usize,
    pub cell_datatype: CellSize,
}

fn parse_cli_args() -> Result<ProgramLaunchState, String> {
    let args = env::args().collect::<Vec<_>>();
    let mut launch_state = ProgramLaunchState::default();
    let mut i = 1;

    while i < args.len() {
        match args[i].as_str() {
            "-i" | "--interactive" => {
                if launch_state.interactive || launch_state.input.is_some() {
                    return Err("Interactive flag given when either already given, or when file input or text input flag is set.".to_string());
                }
                launch_state.interactive = true;
            }
            "-m" | "--machine" => {
                if launch_state.code.is_some() {
                    return Err("Machine code input already given.".to_string());
                }
                i += 1;
                let path = args.get(i).ok_or("Expected machine path argument.")?;
                let file_content = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
                launch_state.code = Some(file_content);
            }
            "-c" | "--code" => {
                if launch_state.code.is_some() {
                    return Err("Machine code input already given.".to_string());
                }
                i += 1;
                let code = args.get(i).ok_or("Expected machine code argument.")?;
                launch_state.code = Some(code.to_string());
            }
            "-if" | "--input-file" => {
                if launch_state.input.is_some() {
                    return Err("Input file input already given.".to_string());
                }
                i += 1;
                let path = args.get(i).ok_or("Expected input file path argument.")?;
                let file_content = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
                launch_state.input = Some(file_content);
            }
            "-it" | "--input-text" => {
                if launch_state.input.is_some() {
                    return Err("Input text input already given.".to_string());
                }
                i += 1;
                let text = args.get(i).ok_or("Expected input text argument.")?;
                launch_state.input = Some(text.to_string());
            }
            "-s" | "--memory-size" => {
                if launch_state.memory_size.is_some() {
                    return Err("Memory size input already given.".to_string());
                }
                i += 1;
                let size = args
                    .get(i)
                    .ok_or("Expected memory size argument.")?
                    .parse::<usize>()
                    .map_err(|e| e.to_string())?;
                launch_state.memory_size = Some(size);
            }
            "-cs" | "--cell-size" => {
                if launch_state.cell_datatype.is_some() {
                    return Err("Cell size input already given.".to_string());
                }
                i += 1;
                let size = args.get(i).ok_or("Expected cell size argument.")?.as_str();
                let size = match size {
                    "8" => CellSize::I8,
                    "16" => CellSize::I16,
                    "32" => CellSize::I32,
                    "64" => CellSize::I64,
                    _ => {
                        return Err("Unknown cell size. Supported sizes: 8, 16, 32, 64".to_string())
                    }
                };
                launch_state.cell_datatype = Some(size);
            }
            "-bits8" | "-bit8" | "-b8" | "--bits8" | "--bit8" | "--b8" => {
                if launch_state.cell_datatype.is_some() {
                    return Err("Cell size input already given.".to_string());
                }
                launch_state.cell_datatype = Some(CellSize::I8);
            }
            "-bits16" | "-bit16" | "-b16" | "--bits16" | "--bit16" | "--b16" => {
                if launch_state.cell_datatype.is_some() {
                    return Err("Cell size input already given.".to_string());
                }
                launch_state.cell_datatype = Some(CellSize::I16);
            }
            "-bits32" | "-bit32" | "-b32" | "--bits32" | "--bit32" | "--b32" => {
                if launch_state.cell_datatype.is_some() {
                    return Err("Cell size input already given.".to_string());
                }
                launch_state.cell_datatype = Some(CellSize::I32);
            }
            "-bits64" | "-bit64" | "-b64" | "--bits64" | "--bit64" | "--b64" => {
                if launch_state.cell_datatype.is_some() {
                    return Err("Cell size input already given.".to_string());
                }
                launch_state.cell_datatype = Some(CellSize::I64);
            }
            _ => {
                return Err(format!("Unknown argument: {}", args[i]));
            }
        }
        i += 1;
    }

    Ok(launch_state)
}

fn resolve_launch_state(launch_state: ProgramLaunchState) -> Result<ResolvedLaunchState, String> {
    Ok(ResolvedLaunchState {
        interactive: launch_state.interactive,
        code: launch_state
            .code
            .ok_or("No machine code given.".to_string())?,
        input: launch_state.input,
        memory_size: launch_state.memory_size.unwrap_or(1_000_000),
        cell_datatype: launch_state.cell_datatype.unwrap_or(CellSize::I16),
    })
}

enum CursorPos {
    MachineInput,
    CommandInput,
    TapeView(usize),
}

#[derive(PartialEq, Eq)]
enum SimulationState {
    Paused,
    Step(u64),
    Run,
}

fn main() -> io::Result<()> {
    let launch_state = resolve_launch_state(parse_cli_args().unwrap()).unwrap();

    if launch_state.interactive {
        run_interactive(launch_state)
    } else {
        run(launch_state)
    }
}

fn run(launch_state: ResolvedLaunchState) -> io::Result<()> {
    let mut machine = MachineType::Running(
        compile_bf(
            &launch_state.code,
            launch_state.memory_size,
            launch_state.cell_datatype,
        )
        .unwrap(),
    );

    let input = launch_state.input.expect("No input !");
    let mut iter = input.chars();

    loop {
        for c in machine.clear_stdout() {
            print!("{}", c);
            stdout().flush()?;
        }

        match machine {
            MachineType::Running(r) => {
                machine = r.step();
            }
            MachineType::Waiting(w) => match iter.next() {
                Some(value) => {
                    machine = MachineType::Running(w.input(value as u8 as i64));
                }
                None => panic!("No more input !"),
            },
            MachineType::Halted(_) => {
                return Ok(());
            }
            MachineType::Crashed(_, err) => {
                panic!("Machine crashed: {:?}", err)
            }
        }
    }
}

fn run_interactive(launch_state: ResolvedLaunchState) -> io::Result<()> {
    let mut stdout = stdout();
    terminal::enable_raw_mode()?;

    execute!(
        stdout,
        terminal::Clear(ClearType::All),
        cursor::MoveTo(0, 0),
        cursor::Hide,
    )?;

    let mut interactive_input_lines = VecDeque::<VecDeque<i64>>::new();
    let mut interactive_input = VecDeque::<i64>::new();

    let mut machine = MachineType::Running(
        compile_bf(
            &launch_state.code,
            launch_state.memory_size,
            launch_state.cell_datatype,
        )
        .unwrap(),
    );

    let mut last_update = SystemTime::now();
    let mut last_update_internal = SystemTime::now();
    let mut cursor_pos = CursorPos::MachineInput;
    let mut command_input = Vec::new();
    let mut sim_state = SimulationState::Paused;

    let mut total_logs: VecDeque<Option<Vec<char>>> = VecDeque::new();
    let mut first_iter = true;

    'outer: loop {
        let mut interactive_input_updated = first_iter;
        first_iter = false;

        if SystemTime::now()
            .duration_since(last_update)
            .unwrap()
            .as_millis()
            > 30
        {
            last_update = SystemTime::now();

            while event::poll(std::time::Duration::from_secs(0))? {
                match event::read()? {
                    Event::Key(key_event) => {
                        if key_event.kind == KeyEventKind::Release {
                            continue;
                        }
                        match key_event.code {
                            KeyCode::Char('c') | KeyCode::Char('C') => {
                                if key_event.modifiers.contains(KeyModifiers::CONTROL) {
                                    break 'outer;
                                }
                            }
                            KeyCode::Tab => {
                                cursor_pos = match cursor_pos {
                                    CursorPos::MachineInput => CursorPos::CommandInput,
                                    CursorPos::CommandInput => match sim_state {
                                        SimulationState::Paused => CursorPos::TapeView(
                                            *machine.get_state().get_instruction_pointer(),
                                        ),
                                        _ => CursorPos::MachineInput,
                                    },
                                    CursorPos::TapeView(_) => CursorPos::MachineInput,
                                };
                                interactive_input_updated = true;
                            }
                            _ => {}
                        }

                        match cursor_pos {
                            CursorPos::MachineInput => match key_event.code {
                                KeyCode::Char(c) => {
                                    interactive_input.push_back(c as u8 as i64);
                                    interactive_input_updated = true;
                                }
                                KeyCode::Backspace => {
                                    interactive_input.pop_back();
                                    interactive_input_updated = true;
                                }
                                KeyCode::Enter => {
                                    interactive_input.push_back(b'\n' as i64);
                                    interactive_input_lines.push_back(
                                        interactive_input.iter().map(|c| *c as u8 as i64).collect(),
                                    );
                                    interactive_input = VecDeque::new();
                                    interactive_input_updated = true;
                                }
                                _ => {}
                            },
                            CursorPos::CommandInput => match key_event.code {
                                KeyCode::Esc => {
                                    sim_state = SimulationState::Paused;
                                    interactive_input_updated = true;
                                }
                                KeyCode::Char(c) => {
                                    if sim_state == SimulationState::Paused {
                                        command_input.push(c);
                                    }
                                    interactive_input_updated = true;
                                }
                                KeyCode::Backspace => {
                                    if sim_state == SimulationState::Paused {
                                        command_input.pop();
                                    }
                                    interactive_input_updated = true;
                                }
                                KeyCode::Enter => {
                                    if command_input.len() == 1 {
                                        match command_input.first().unwrap() {
                                            's' => sim_state = SimulationState::Step(1),
                                            'c' => sim_state = SimulationState::Run,
                                            _ => {}
                                        }
                                    } else if !command_input.is_empty() {
                                        let command = command_input.iter().collect::<String>();
                                        let mut parts = command.split_whitespace();
                                        if let Some(first) = parts.next() {
                                            if first == "s" {
                                                if let Some(second) = parts.next() {
                                                    if let Ok(num) = second.parse::<u64>() {
                                                        sim_state = SimulationState::Step(num);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    command_input = Vec::new();

                                    interactive_input_updated = true;
                                }
                                _ => {}
                            },
                            CursorPos::TapeView(pos) => match key_event.code {
                                KeyCode::Left => {
                                    if pos > 0 {
                                        cursor_pos = CursorPos::TapeView(pos - 1);
                                        interactive_input_updated = true;
                                    }
                                }
                                KeyCode::Right => {
                                    if pos < machine.get_state().get_memory().len() - 1 {
                                        cursor_pos = CursorPos::TapeView(pos + 1);
                                        interactive_input_updated = true;
                                    }
                                }
                                _ => {}
                            },
                        }
                    }
                    Event::Resize(_, _) => {
                        execute!(stdout, terminal::Clear(ClearType::All))?;
                        interactive_input_updated = true;
                    }
                    _ => {}
                }
            }
        }

        if interactive_input_updated {
            let (w, _) = terminal::size()?;

            execute!(stdout, ResetColor, cursor::MoveTo(0, 0))?;

            macro_rules! print_input_field {
                ($name: expr, $selected: expr, $text_source: expr) => {{
                    if $selected {
                        execute!(stdout, SetBackgroundColor(Color::Blue))?;
                    } else {
                        execute!(stdout, ResetColor)?;
                    }
                    execute!(stdout, terminal::Clear(ClearType::CurrentLine),)?;
                    write!(stdout, "{} > ", $name)?;

                    let cur_col = ($name.len() as isize) + 3;
                    let available = (w as isize) - cur_col;
                    let start_idx = ($text_source.len() as isize) - available;
                    if start_idx < 0 {
                        write!(
                            stdout,
                            "{}",
                            $text_source
                                .iter()
                                .map(|v| *v as u8 as char)
                                .collect::<String>()
                        )?;
                    } else {
                        write!(
                            stdout,
                            "{}",
                            $text_source
                                .iter()
                                .skip(start_idx as usize)
                                .map(|v| *v as u8 as char)
                                .collect::<String>()
                        )?;
                    }

                    execute!(
                        stdout,
                        ResetColor,
                        cursor::MoveToNextLine(1),
                        cursor::MoveToColumn(0)
                    )?;
                }};
            }

            macro_rules! write_separator {
                () => {
                    write!(
                        stdout,
                        "{}",
                        vec!['-'; w as usize].iter().collect::<String>()
                    )?;

                    execute!(stdout, cursor::MoveToNextLine(1), cursor::MoveToColumn(0))?;
                };
            }

            write_separator!();
            print_input_field!(
                "INPUT",
                matches!(cursor_pos, CursorPos::MachineInput),
                interactive_input
            );
            write_separator!();
            print_input_field!(
                "CMD",
                matches!(cursor_pos, CursorPos::CommandInput),
                command_input
            );
            write_separator!();
            writeln!(stdout)?;
        }

        if sim_state != SimulationState::Paused {
            match machine {
                MachineType::Waiting(waiting) => {
                    if launch_state.interactive {
                        match interactive_input_lines.front_mut() {
                            Some(front) => match front.pop_front() {
                                Some(value) => {
                                    machine = MachineType::Running(waiting.input(value));
                                    if front.is_empty() {
                                        interactive_input_lines.pop_front();
                                    }
                                }
                                None => {
                                    machine = MachineType::Waiting(waiting);
                                }
                            },
                            None => match interactive_input.pop_front() {
                                Some(value) => {
                                    machine = MachineType::Running(waiting.input(value));
                                    interactive_input_updated = true;
                                }
                                None => {
                                    machine = MachineType::Waiting(waiting);
                                }
                            },
                        }
                    } else {
                        machine = MachineType::Waiting(waiting);
                    }
                }
                MachineType::Running(running) => {
                    machine = running.step();

                    if let SimulationState::Step(count) = sim_state {
                        if count == 1 {
                            sim_state = SimulationState::Paused;
                            interactive_input_updated = true;
                        } else {
                            sim_state = SimulationState::Step(count - 1);
                        }
                    }
                }
                _ => {}
            }

            let logs = machine.clear_stdout();

            if !logs.is_empty() {
                while total_logs.len() > 50 {
                    total_logs.pop_front();
                }
                if total_logs.is_empty() {
                    total_logs.push_back(None);
                }
                for log in logs {
                    match total_logs.back_mut() {
                        Some(Some(line)) => {
                            if log == '\n' {
                                total_logs.push_back(None);
                            } else {
                                line.push(log);
                            }
                        }
                        Some(o) => {
                            if log != '\n' {
                                *o = Some(vec![log]);
                            }
                        }
                        None => unreachable!(),
                    };
                }
                interactive_input_updated = true;
            }
        }

        match machine {
            MachineType::Crashed(..) | MachineType::Halted(..) => {
                interactive_input_updated = true;
                sim_state = SimulationState::Paused;
            }
            _ => {}
        }

        if (sim_state == SimulationState::Paused
            && SystemTime::now()
                .duration_since(last_update_internal)
                .unwrap()
                .as_millis()
                > 500)
            || interactive_input_updated
        {
            last_update_internal = SystemTime::now();
            execute!(stdout, cursor::MoveTo(0, 6))?;
            let (w, h) = terminal::size()?;

            let tape_entry_size = match launch_state.cell_datatype {
                CellSize::I8 => 2,
                CellSize::I16 => 4,
                CellSize::I32 => 8,
                CellSize::I64 => 16,
            } + 1;

            let max_tape_on_line = ((w - 1) as usize) / tape_entry_size;
            let tape_on_left = max_tape_on_line / 2;

            let state = machine.get_state();
            let pointer = *state.get_pointer();
            let tape_view_center = match cursor_pos {
                CursorPos::TapeView(selected_pos) => selected_pos,
                _ => pointer,
            };
            let tape_window_start = if tape_view_center > tape_on_left {
                tape_view_center - tape_on_left
            } else {
                0
            };
            let tape_window_end =
                (tape_window_start + max_tape_on_line).min(state.get_memory().len());
            let tape_slice = &state.get_memory()[tape_window_start..tape_window_end];

            let instruction_pointer = *state.get_instruction_pointer();
            let instructions_window_start = if instruction_pointer > (w as usize) {
                instruction_pointer - (w as usize)
            } else {
                0
            };
            let instructions_window_end =
                (instruction_pointer + (w as usize)).min(state.get_instructions().len());
            let instructions_slice =
                &state.get_instructions()[instructions_window_start..instructions_window_end];

            let tape_len = (tape_slice.len() * tape_entry_size + 1) as u16;

            execute!(
                stdout,
                terminal::Clear(ClearType::CurrentLine),
                cursor::MoveToColumn((w - tape_len) / 2)
            )?;
            write!(
                stdout,
                "+{}+",
                vec!['-'; (tape_len as isize - 2).max(0) as usize]
                    .iter()
                    .collect::<String>()
            )?;
            let title = format!("TAPE DUMP HEX. ADDR={}", tape_view_center);
            execute!(
                stdout,
                cursor::MoveToColumn((w - title.len() as u16) / 2),
                SetForegroundColor(Color::DarkYellow)
            )?;
            write!(stdout, "{}", title)?;
            execute!(
                stdout,
                ResetColor,
                cursor::MoveToNextLine(1),
                cursor::MoveToColumn(0)
            )?;

            execute!(
                stdout,
                terminal::Clear(ClearType::CurrentLine),
                ResetColor,
                cursor::MoveToColumn((w - tape_len) / 2)
            )?;

            write!(stdout, "|")?;
            for (i, &tape_value) in tape_slice.iter().enumerate() {
                let hex = match launch_state.cell_datatype {
                    CellSize::I8 => format!("{:02x}", tape_value),
                    CellSize::I16 => format!("{:04x}", tape_value),
                    CellSize::I32 => format!("{:08x}", tape_value),
                    CellSize::I64 => format!("{:016x}", tape_value),
                };

                let selected = if let CursorPos::TapeView(selected_pos) = cursor_pos {
                    selected_pos - tape_window_start == i
                } else {
                    false
                };
                if i != 0 {
                    write!(stdout, " ")?;
                }
                if pointer - tape_window_start == i {
                    execute!(stdout, ResetColor, SetForegroundColor(Color::Green))?;
                    if selected {
                        execute!(stdout, SetBackgroundColor(Color::Blue))?;
                    }
                    write!(stdout, "{}", hex)?;
                } else {
                    if selected {
                        execute!(stdout, SetBackgroundColor(Color::Blue))?;
                    }
                    write!(stdout, "{}", hex)?;
                }
                execute!(stdout, ResetColor)?;
            }
            write!(stdout, "|")?;
            execute!(
                stdout,
                cursor::MoveToNextLine(1),
                terminal::Clear(ClearType::CurrentLine),
                cursor::MoveToColumn((w - tape_len) / 2)
            )?;
            write!(
                stdout,
                "+{}+",
                vec!['-'; (tape_len as isize - 2).max(0) as usize]
                    .iter()
                    .collect::<String>()
            )?;
            execute!(stdout, cursor::MoveToNextLine(3), cursor::MoveToColumn(0))?;

            let instructions_as_str = instructions_slice
                .iter()
                .map(|inst| inst.to_string())
                .collect::<Vec<_>>();

            let middle_inst = instructions_as_str
                .get(instruction_pointer - instructions_window_start)
                .map(|v| v.as_str())
                .unwrap_or("");
            let middle_inst_len = middle_inst.len() as u16;
            execute!(
                stdout,
                terminal::Clear(ClearType::CurrentLine),
                cursor::MoveToColumn((w - middle_inst_len) / 2),
                SetForegroundColor(Color::Green)
            )?;
            write!(stdout, "{}", middle_inst)?;
            execute!(stdout, ResetColor)?;

            let mut curr_col = (w - middle_inst_len) / 2 + middle_inst_len;

            for inst in instructions_as_str
                .iter()
                .skip(instruction_pointer - instructions_window_start + 1)
            {
                if curr_col + (inst.len() + 1) as u16 > w - 3 {
                    break;
                }
                curr_col += (inst.len() + 1) as u16;
                write!(stdout, " {}", inst)?;
            }

            curr_col = (w - middle_inst_len) / 2 - 1;
            for inst in instructions_as_str
                .iter()
                .take(instruction_pointer - instructions_window_start)
                .rev()
            {
                if curr_col - (inst.len() + 1) as u16 <= 2 {
                    break;
                }
                curr_col -= (inst.len() + 1) as u16;
                execute!(stdout, cursor::MoveToColumn(curr_col))?;
                write!(stdout, " {}", inst)?;
            }

            execute!(
                stdout,
                cursor::MoveToColumn(2),
                cursor::MoveUp(1),
                terminal::Clear(ClearType::CurrentLine)
            )?;
            write!(
                stdout,
                "+{}+",
                vec!['-'; w as usize - 6].iter().collect::<String>()
            )?;
            let title = "INSTRUCTION DUMP";
            execute!(
                stdout,
                cursor::MoveToColumn((w - title.len() as u16) / 2),
                SetForegroundColor(Color::DarkYellow)
            )?;
            write!(stdout, "{}", title)?;
            execute!(
                stdout,
                ResetColor,
                cursor::MoveToNextLine(1),
                cursor::MoveToColumn(2)
            )?;
            write!(stdout, "|")?;
            execute!(stdout, cursor::MoveToColumn(w - 3))?;
            write!(stdout, "|")?;
            execute!(
                stdout,
                cursor::MoveToColumn(2),
                cursor::MoveDown(1),
                terminal::Clear(ClearType::CurrentLine)
            )?;
            write!(
                stdout,
                "+{}+",
                vec!['-'; w as usize - 6].iter().collect::<String>()
            )?;

            execute!(stdout, cursor::MoveToNextLine(2), ResetColor)?;
            write!(
                stdout,
                "{}",
                vec!['#'; w as usize].iter().collect::<String>()
            )?;
            let title = "  MACHINE OUTPUT  ";
            execute!(
                stdout,
                cursor::MoveToColumn((w - title.len() as u16) / 2),
                SetForegroundColor(Color::DarkYellow)
            )?;
            write!(stdout, "{}", title)?;
            execute!(
                stdout,
                ResetColor,
                cursor::MoveToNextLine(1),
                cursor::MoveToColumn(0)
            )?;

            let remaining_lines = h - 16;
            let mut effective_lines = Vec::new();
            for line in total_logs.iter().flatten() {
                line.chunks(w as usize)
                    .map(|chunk| chunk.iter().collect::<String>())
                    .for_each(|v| effective_lines.push(v));
            }

            for line in effective_lines
                .iter()
                .rev()
                .take(remaining_lines as usize)
                .rev()
            {
                writeln!(stdout, "{}", line)?;
            }

            stdout.flush()?;
        }

        match machine {
            MachineType::Crashed(_, reason) => {
                execute!(stdout, cursor::MoveToNextLine(2))?;
                writeln!(stdout, "Machine crashed: {:?}", reason)?;
                stdout.flush()?;
                break;
            }
            MachineType::Halted(_) => {
                execute!(stdout, cursor::MoveToNextLine(2))?;
                writeln!(stdout, "Machine halted")?;
                stdout.flush()?;
                break;
            }
            _ => {}
        }

        stdout.flush()?;
    }

    execute!(stdout, cursor::Show)?;
    terminal::disable_raw_mode()?;
    stdout.flush()?;
    println!("\n\nExit.");

    Ok(())
}
