; =============================================================================
; INT 13h — Disk (emulator trap)
; =============================================================================
int13_handler:
    sti                     ; the caller gets its interrupts back, as on a real BIOS
    out 0xB0, al
    retf 2
