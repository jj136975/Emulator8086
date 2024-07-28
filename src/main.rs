use std::fs::File;

use clap::Parser;
use clap_derive::Parser;
use log::{logger, set_logger, SetLoggerError};

use crate::a_out::executable::Executable;
use crate::vm::runtime::Runtime;

mod a_out;
mod vm;

#[derive(Parser, Debug)]
struct CLI {
    path: std::path::PathBuf,
}

fn main() {
    let args = CLI::parse();
    match File::open(args.path) {
        Ok(mut file) => match Executable::from_reader(&mut file) {
            Ok(mut exe) => {
                println!("{:?}", exe);
                env_logger::init();
                let mut runtime = Runtime::new(&mut exe);
                runtime.run();
            },
            Err(error) => eprintln!("{:?}", error)
        },
        Err(error) => eprintln!("{:?}", error),
    }
}
