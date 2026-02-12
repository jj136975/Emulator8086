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
mod io;
mod bios;

#[derive(Parser, Debug)]
#[command(name = "emulator8086", about = "Intel 8086 emulator")]
struct CLI {
    /// Path to a.out executable (for MINIX mode)
    path: Option<std::path::PathBuf>,
    /// Boot in BIOS mode
    #[arg(long)]
    bios: bool,
    /// Path to floppy disk image (for BIOS mode)
    #[arg(long)]
    disk: Option<std::path::PathBuf>,
    /// Enable register trace output (MINIX mode)
    #[arg(long)]
    trace: bool,
    /// Arguments to pass to the a.out executable
    args: Vec<String>,
}

fn main() {
    env_logger::init();
    let mut args = CLI::parse();

    if args.bios {
        let disk_path = args.disk.expect("--disk is required for BIOS mode");
        let mut runtime = Runtime::new_bios(&disk_path);
        runtime.run();
    } else {
        let path = args.path.expect("Path to a.out executable is required");
        args.args.insert(0, env::args().collect::<Vec<String>>()[1].clone());
        match File::open(&path) {
            Ok(mut file) => match Executable::from_reader(&mut file) {
                Ok(exe) => {
                    let mut runtime = Runtime::new(&exe, args.args);
                    runtime.trace = args.trace;
                    if args.trace {
                        eprintln!(" AX   BX   CX   DX   SP   BP   SI   DI  FLAGS IP");
                    }
                    runtime.run();
                },
                Err(error) => eprintln!("{:?}", error)
            },
            Err(error) => eprintln!("{:?}", error),
        }
    }
}
