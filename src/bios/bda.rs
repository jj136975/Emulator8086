// BIOS Data Area (BDA) — physical addresses (segment 0x0040 base = 0x400)

pub const EQUIP_WORD: usize = 0x410;       // Equipment word (2 bytes)
pub const MEMORY_SIZE_KB: usize = 0x413;   // Conventional memory in KB (2 bytes)
pub const KB_SHIFT_FLAGS: usize = 0x417;   // Keyboard shift state
pub const KB_HEAD: usize = 0x41A;          // Keyboard buffer head (segment offset, word)
pub const KB_TAIL: usize = 0x41C;          // Keyboard buffer tail (segment offset, word)
pub const KB_BUFFER_START: usize = 0x41E;  // Ring buffer start (32 bytes = 16 entries)
pub const KB_BUFFER_END: usize = 0x43E;    // Ring buffer end (exclusive)
pub const FLOPPY_STATUS: usize = 0x441;    // Last floppy disk status byte
pub const HD_STATUS: usize = 0x474;        // Last hard disk status byte
pub const VIDEO_MODE: usize = 0x449;       // Current video mode
pub const VIDEO_COLS: usize = 0x44A;       // Screen columns (word)
pub const CURSOR_POS: usize = 0x450;       // Cursor positions array (8 pages x 2 bytes)
pub const CURSOR_SHAPE: usize = 0x460;     // Cursor start/end scan lines (word)
pub const ACTIVE_PAGE: usize = 0x462;      // Current display page
pub const CRTC_PORT: usize = 0x463;        // CRTC base port (word)
pub const TICK_COUNT: usize = 0x46C;       // Timer ticks since midnight (dword)
pub const TICK_OVERFLOW: usize = 0x470;    // Midnight rollover flag
pub const HD_COUNT: usize = 0x475;         // Number of hard disks
pub const VIDEO_ROWS: usize = 0x484;       // Text rows minus 1
pub const CHAR_HEIGHT: usize = 0x485;      // Character height in scan lines (word)
pub const KB_STATUS_3: usize = 0x496;      // Keyboard status 3 (E0 prefix flag)

// Segment-relative offsets for the keyboard ring buffer head/tail pointers
// (BDA stores these as offsets from segment 0x0040, not physical addresses)
pub const KB_BUF_OFFSET_START: u16 = 0x1E;
pub const KB_BUF_OFFSET_END: u16 = 0x3E;
