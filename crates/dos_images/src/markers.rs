//! What the guest tells the host on the port E9h, and the program it tells
//! it with. The batch files the tool writes run E9.COM at each step with
//! the words of a `Marker`, and the host reads the lines of the port for
//! them.
//!
//! The invariant: a marker is a line of its own on the port. The BIOS
//! copies whatever it prints through INT 10h to the port, and leaves a line
//! open whenever a program prints no line feed: E9.COM ends any line that is
//! open before it writes its own, and a line is a marker only when the
//! whole line is one (`Marker::parse`).

/// E9.COM: writes a line feed to the port E9h, then its command tail, then
/// a line feed, and ends.
pub(crate) const E9_COM: [u8; 30] = [
    0xFC, // cld: the tail is read upwards, whatever direction DOS left
    0xBE, 0x81, 0x00, // mov si, 81h: the command tail
    0x8A, 0x0E, 0x80, 0x00, // mov cl, [80h]: its length
    0x30, 0xED, // xor ch, ch
    0xB0, 0x0A, // mov al, 0Ah
    0xE6, 0xE9, // out 0E9h, al: the line open on the port ends
    0xE3, 0x05, // jcxz done
    0xAC, // next: lodsb
    0xE6, 0xE9, // out 0E9h, al
    0xE2, 0xFB, // loop next
    0xB0, 0x0A, // done: mov al, 0Ah
    0xE6, 0xE9, // out 0E9h, al: the line of the tail ends
    0xB8, 0x00, 0x4C, // mov ax, 4C00h
    0xCD, 0x21, // int 21h: terminate with return code 0
];

/// A step a batch file of the tool tells the host of.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Marker {
    /// BUILD.BAT starts: FreeDOS is up.
    BuildStart,
    /// NMAKE starts in the module, a directory below the source's root.
    Module(String),
    /// NMAKE ended with an error in the module.
    ModuleFailed(String),
    /// Every module was built: CPY.BAT copies what they made.
    Copying,
    /// BUILD.BAT ends.
    BuildDone,
    /// The batch file of a check of a disk ends.
    ChecksDone,
}

impl Marker {
    /// The words a batch file gives E9.COM for the marker: they come back
    /// as the marker's line.
    pub(crate) fn words(&self) -> String {
        match self {
            Self::BuildStart => "BUILD START".to_string(),
            Self::Module(module) => format!("MODULE {module}"),
            Self::ModuleFailed(module) => format!("MODULE-FAILED {module}"),
            Self::Copying => "BUILD COPY".to_string(),
            Self::BuildDone => "BUILD DONE".to_string(),
            Self::ChecksDone => "CHECKS DONE".to_string(),
        }
    }

    /// The marker the line of the port is, if the whole line is one. DOS
    /// hands a program its command tail with the blank before it.
    pub(crate) fn parse(line: &str) -> Option<Self> {
        let words = line.trim();
        let module = |name: &str| {
            let plain = !name.is_empty() && !name.contains(char::is_whitespace);
            plain.then(|| name.to_string())
        };
        match words {
            "BUILD START" => Some(Self::BuildStart),
            "BUILD COPY" => Some(Self::Copying),
            "BUILD DONE" => Some(Self::BuildDone),
            "CHECKS DONE" => Some(Self::ChecksDone),
            _ => match words.split_once(' ') {
                Some(("MODULE", name)) => module(name).map(Self::Module),
                Some(("MODULE-FAILED", name)) => module(name).map(Self::ModuleFailed),
                _ => None,
            },
        }
    }
}

#[cfg(test)]
#[path = "markers_tests.rs"]
mod tests;
