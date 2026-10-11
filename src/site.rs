//! THE table of the files the site publishes beside the page, for the app
//! to fetch (`platform::fetch`): where each lies, relative to the page, what
//! it is, which says what the app does with it, and what the user knows it
//! by. Only a host that came from a site has them (`platform::HAS_SITE`).
//!
//! The invariant: the site publishes every file of the table beside the
//! page, and nothing else there. A ROM is the repository's: the page has
//! trunk copy it into what trunk builds (`index.html`), and the workflow
//! that publishes the site checks it there. A disk is built by that
//! workflow (`crates/dos_images`), which copies it into what trunk built:
//! the page names no disk, so that `trunk serve` builds the page of the
//! repository alone, and serves no disk. A test reads the page and the
//! workflow (`site_tests.rs`).

/// What a file of the site is, which says what the app does with it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    /// A ROM image, which the machine can restart with (Machine > Load
    /// ROM).
    Rom,
    /// A floppy image, which the machine can boot from drive A:.
    Floppy,
    /// A hard disk image, which the machine can boot as its first hard
    /// disk, C:.
    HardDisk,
}

/// A file the site publishes beside the page.
#[derive(Debug, PartialEq, Eq)]
pub struct SiteFile {
    /// Where it lies, relative to the page.
    pub path: &'static str,
    pub kind: Kind,
    /// What the user knows it by: the entry of the menu, or the button,
    /// that loads it.
    pub title: &'static str,
}

/// The files of the site.
pub static FILES: [SiteFile; 3] = [
    SiteFile {
        path: "rom/bios.bin",
        kind: Kind::Rom,
        title: "BIOS from the site",
    },
    SiteFile {
        path: "disks/msdos400.img",
        kind: Kind::Floppy,
        title: "MS-DOS 4.00",
    },
    SiteFile {
        path: "disks/doom8088.img",
        kind: Kind::HardDisk,
        title: "DOOM",
    },
];

/// The files of the site that are `kind`, in the order of the table.
pub fn of_kind(kind: Kind) -> impl Iterator<Item = &'static SiteFile> {
    FILES.iter().filter(move |file| file.kind == kind)
}

#[cfg(test)]
#[path = "site_tests.rs"]
mod tests;
