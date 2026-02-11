use std::ffi::{CStr, CString};
use std::io::Write;
use std::ptr::slice_from_raw_parts_mut;
use num_derive::FromPrimitive;

use log::{debug, error};
use num_traits::FromPrimitive;
use crate::minix2::interruption::Message;
use crate::vm::runtime::Runtime;

#[repr(u16)]
#[derive(Debug, FromPrimitive)]
pub enum SyscallInterrupt {
    EXIT = 1,
    FORK = 2,
    READ = 3,
    WRITE = 4,
    OPEN = 5,
    CLOSE = 6,
    WAIT = 7,
    CREAT = 8,
    LINK = 9,
    UNLINK = 10,
    WAITPID = 11,
    CHDIR = 12,
    TIME = 13,
    MKNOD = 14,
    CHMOD = 15,
    CHOWN = 16,
    BRK = 17,
    STAT = 18,
    LSEEK = 19,
    GETPID = 20,
    MOUNT = 21,
    UMOUNT = 22,
    SETUID = 23,
    GETUID = 24,
    STIME = 25,
    PTRACE = 26,
    ALARM = 27,
    FSTAT = 28,
    PAUSE = 29,
    UTIME = 30,
    ACCESS = 33,
    SYNC = 36,
    KILL = 37,
    RENAME = 38,
    MKDIR = 39,
    RMDIR = 40,
    DUP = 41,
    PIPE = 42,
    TIMES = 43,
    SETGID = 46,
    GETGID = 47,
    SIGNAL = 48,
    IOCTL = 54,
    FCNTL = 55,
    EXEC = 59,
    UMASK = 60,
    CHROOT = 61,
    SETSID = 62,
    GETPGRP = 63,

    /* The following are not system calls, but are processed like them. */
    KSIG = 64,
    /* kernel detected a signal */
    UNPAUSE = 65,
    /* to MM or FS: check for EINTR */
    REVIVE = 67,
    /* to FS: revive a sleeping process */
    TASK_REPLY = 68,
    /* to FS: reply code from tty task */

    /* Posix signal handling. */
    SIGACTION = 71,
    SIGSUSPEND = 72,
    SIGPENDING = 73,
    SIGPROCMASK = 74,
    SIGRETURN = 75,

    REBOOT = 76,
    SVRCTL = 77,
}

fn minix_write(vm: &mut Runtime, fd: u16, addr: u16, len: u16) -> usize {
    debug!("<write({}, 0x{:04x}, {})", fd, addr, len);
    let bytes: &[u8] = unsafe {
        std::slice::from_raw_parts(vm.registers.ds.as_ptr_at(addr), len as usize)
    };

    let ret = match fd {
        0 => { error!("Cannot write to STDIN"); 0 },
        1 => {
            let s = String::from_utf8_lossy(bytes);
            print!("{}", s);
            std::io::stdout().flush().ok();
            bytes.len()
        },
        2 => {
            let s = String::from_utf8_lossy(bytes);
            eprint!("{}", s);
            std::io::stderr().flush().ok();
            bytes.len()
        },
        _ => 0
    };
    debug!(" => {}>\n", ret);
    vm.registers.ax.set(0);

    return ret;
}

fn minix_brk(vm: &mut Runtime, addr: u16) -> u16 {
    debug!("<brk(0x{:04x})>\n", addr);
    // Accept any break within the 64KB DS segment
    0
}

fn minix_ioctl(vm: &mut Runtime, fd: u16, request: u16) -> u16 {
    debug!("<ioctl({}, 0x{:04x})>\n", fd, request);
    // Return -EINVAL (emulator cannot perform real ioctl)
    (-22i16) as u16
}

fn minix_exit(vm: &mut Runtime, status: u16) {
    debug!("<exit({})>\n", status);
    vm.exit(status);
}

pub fn handle_interrupt(vm: &mut Runtime, message: &mut Message) {
    let m_type = message.m_type;
    match SyscallInterrupt::from_u16(m_type) {
        Some(syscall) => match syscall {
            SyscallInterrupt::EXIT => unsafe { minix_exit(vm, message.m_u.m1.i1 ) },
            SyscallInterrupt::READ => {

            },
            SyscallInterrupt::WRITE => {
                let ret = unsafe { minix_write(vm, message.m_u.m1.i1, message.m_u.m1.p1, message.m_u.m1.i2) };
                message.m_type = ret as u16;
            },
            SyscallInterrupt::OPEN => {

            },
            SyscallInterrupt::BRK => {
                let addr = unsafe { message.m_u.m1.p1 };
                let ret = minix_brk(vm, addr);
                message.m_type = ret;
                unsafe { message.m_u.m2.p1 = addr };
            },
            SyscallInterrupt::IOCTL => {
                let ret = unsafe { minix_ioctl(vm, message.m_u.m1.i1, message.m_u.m1.i3) };
                message.m_type = ret;
            },
            _ => error!("Not handled syscall: {}", m_type),
        },
        None => error!("Unknown syscall: {}", m_type),
    }
}