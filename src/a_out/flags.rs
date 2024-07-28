
/* Flags. */
const UZP: u8 = 0x01;   /* unmapped zero page (pages) */
const PAL: u8 = 0x02;   /* page aligned executable */
const NSYM: u8 = 0x04;  /* new style symbol table */
const EXEC: u8 = 0x10;  /* executable */
const SEP: u8 = 0x20;   /* separate I/D */
const PURE: u8 = 0x40;  /* pure text */ /* not used */
const TOVLY: u8 = 0x80; /* text overlay */ /* not used */