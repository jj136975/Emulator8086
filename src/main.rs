use std::env;
use std::fs::File;

use clap::Parser;
use clap_derive::Parser;

use crate::a_out::executable::Executable;
use crate::vm::runtime::Runtime;

mod a_out;
mod vm;
mod utils;
mod minix2;

#[derive(Parser, Debug)]
struct CLI {
    path: std::path::PathBuf,
    args: Vec<String>,
}

fn main() {
    let mut args = CLI::parse();
    args.args.insert(0, env::args().collect::<Vec<String>>()[1].clone());
    match File::open(args.path) {
        Ok(mut file) => match Executable::from_reader(&mut file) {
            Ok(exe) => {
                // println!("{:?}", exe);
                env_logger::init();
                let mut runtime = Runtime::new(&exe, args.args);
                eprintln!(" AX   BX   CX   DX   SP   BP   SI   DI  FLAGS IP");
                runtime.run();
            },
            Err(error) => eprintln!("{:?}", error)
        },
        Err(error) => eprintln!("{:?}", error),
    }
}
