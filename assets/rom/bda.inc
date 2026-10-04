; =============================================================================
; BDA offsets (segment 0x0040)
; =============================================================================
BDA_EQUIPMENT     equ 0x0010
BDA_MEMSIZE       equ 0x0013
BDA_KBD_FLAGS1    equ 0x0017
BDA_KBD_BUF_HEAD  equ 0x001A
BDA_KBD_BUF_TAIL  equ 0x001C
BDA_VIDEO_MODE    equ 0x0049
BDA_VIDEO_COLS    equ 0x004A
BDA_VIDEO_PGSZ    equ 0x004C
BDA_CURSOR_POS    equ 0x0050
BDA_CURSOR_SHAPE  equ 0x0060
BDA_ACTIVE_PAGE   equ 0x0062
BDA_CRTC_BASE     equ 0x0063
BDA_TIMER_LOW     equ 0x006C
BDA_TIMER_HIGH    equ 0x006E
BDA_TIMER_OFLOW   equ 0x0070
BDA_MOTOR_STATUS  equ 0x003F
BDA_MOTOR_TIMEOUT equ 0x0040
BDA_KBD_BUF_START equ 0x0080
BDA_KBD_BUF_END   equ 0x0082
BDA_KBD_FLAGS2    equ 0x0018   ; Keyboard flags byte 2 (held state of toggle keys)
BDA_FLOPPY_STATUS equ 0x0041   ; Floppy disk last status
BDA_HD_STATUS     equ 0x0074   ; Hard disk last status
BDA_HD_COUNT      equ 0x0075   ; Number of hard disks
BDA_CRT_START     equ 0x004E   ; Offset of the active page in video memory
BDA_CRT_MODE_SET  equ 0x0065   ; Copy of the mode control register (port 3D8h)
BDA_CRT_PALETTE   equ 0x0066   ; Copy of the colour select register (port 3D9h)
BDA_BREAK_FLAG    equ 0x0071   ; Bit 7 set when Ctrl-Break was pressed
BDA_RESET_FLAG    equ 0x0072   ; 1234h = restart requested from the keyboard
BDA_ROWS_M1       equ 0x0084   ; Text rows on screen, minus one
BDA_CHAR_HEIGHT   equ 0x0085   ; Character height in scan lines (word)
