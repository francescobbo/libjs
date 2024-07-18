use scanner::Scanner;

mod expr;
mod scanner;
mod token;
mod token_type;

static mut HAD_ERROR: bool = false;

fn main() {
    // Get the first argument passed to the program
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 2 {
        println!("Usage: {} [script]", args[0]);
        return;
    } else if args.len() == 2 {
        run_file(args[1].as_str());
    } else {
        run_prompt();
    }

    unsafe {
        if HAD_ERROR {
            std::process::exit(42);
        }
    }
}

fn run_file(path: &str) {
    let source = std::fs::read_to_string(path).expect("Failed to read file");
    run(source);
}

fn run_prompt() {
    loop {
        let mut line = String::new();
        if std::io::stdin().read_line(&mut line).is_err() {
            break;
        }
        run(line);
    }
}

fn run(source: String) {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    for token in tokens {
        println!("{}", token);
    }
}

pub fn error(line: usize, message: &str) {
    report(line, "", message);
}

fn report(line: usize, location: &str, message: &str) {
    eprintln!("[line {}] Error {}: {}", line, location, message);

    unsafe {
        HAD_ERROR = true;
    }
}