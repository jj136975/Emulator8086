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
