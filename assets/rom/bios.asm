; =============================================================================
; 8086 BIOS ROM — Full handlers
; Assemble: nasm -f bin bios.asm -o bios.bin
; =============================================================================

cpu 8086
bits 16
org 0x0000

; --- BDA offsets (segment 0x0040) ---
%include "bda.inc"


; =============================================================================
; BIOS ENTRY
; =============================================================================
bios_entry:
    cli
    xor ax, ax
    mov ss, ax
    mov sp, 0x7C00

    mov al, 'B'
    out 0xE9, al

    ; Zero IVT + BDA
    xor ax, ax
    mov es, ax
    xor di, di
    mov cx, 0x0280
    cld
    rep stosw

    ; Install IVT
    xor ax, ax
    mov ds, ax

    mov word [0x0020], int08_handler
    mov word [0x0022], 0xF000
    mov word [0x0024], int09_handler
    mov word [0x0026], 0xF000
    mov word [0x0040], int10_handler
    mov word [0x0042], 0xF000
    mov word [0x0044], int11_handler
    mov word [0x0046], 0xF000
    mov word [0x0048], int12_handler
    mov word [0x004A], 0xF000
    mov word [0x004C], int13_handler
    mov word [0x004E], 0xF000
    mov word [0x0050], iret_stub
    mov word [0x0052], 0xF000
    mov word [0x0054], int15_handler
    mov word [0x0056], 0xF000
    mov word [0x0058], int16_handler
    mov word [0x005A], 0xF000
    mov word [0x005C], iret_stub
    mov word [0x005E], 0xF000
    mov word [0x0064], int19_handler
    mov word [0x0066], 0xF000
    mov word [0x0068], int1a_handler
    mov word [0x006A], 0xF000
    mov word [0x0078], floppy_dpt   ; INT 1Eh offset (0x1E * 4 = 0x78)
    mov word [0x007A], 0xF000       ; INT 1Eh segment

    ; Fill remaining IVT
    xor si, si
    mov cx, 256
.fill_ivt:
    cmp word [si], 0
    jne .next
    mov word [si], iret_stub
    mov word [si+2], 0xF000
.next:
    add si, 4
    loop .fill_ivt

    ; Init BDA
    mov ax, 0x0040
    mov ds, ax
    mov word [BDA_MEMSIZE], 640
    mov word [BDA_EQUIPMENT], 0x0021
    mov byte [BDA_VIDEO_MODE], 0x03
    mov word [BDA_VIDEO_COLS], 80
    mov word [BDA_VIDEO_PGSZ], 4096
    mov word [BDA_CURSOR_POS], 0x0000
    mov word [BDA_CURSOR_SHAPE], 0x0607
    mov byte [BDA_ACTIVE_PAGE], 0
    mov word [BDA_CRTC_BASE], 0x03D4
    mov word [BDA_KBD_BUF_HEAD], 0x001E
    mov word [BDA_KBD_BUF_TAIL], 0x001E
    mov word [BDA_KBD_BUF_START], 0x001E
    mov word [BDA_KBD_BUF_END], 0x003E
    mov word [BDA_TIMER_LOW], 0
    mov word [BDA_TIMER_HIGH], 0
    mov byte [BDA_TIMER_OFLOW], 0
    mov byte [BDA_KBD_FLAGS1], 0
    mov byte [BDA_KBD_FLAGS2], 0

    ; BDA offset 0x0075 = number of hard disks
    ; The disk trap will set this correctly, but set a default
    mov byte [0x0075], 1          ; 1 hard disk

    ; Init PIC
    mov al, 0x11
    out 0x20, al
    mov al, 0x08
    out 0x21, al
    mov al, 0x04
    out 0x21, al
    mov al, 0x01
    out 0x21, al
    mov al, 0xFC
    out 0x21, al

    ; Init PIT
    mov al, 0x36
    out 0x43, al
    mov al, 0x00
    out 0x40, al
    out 0x40, al

    ; Clear screen
    mov ax, 0xB800
    mov es, ax
    xor di, di
    mov cx, 2000
    mov ax, 0x0720
    cld
    rep stosw

    mov al, 'I'
    out 0xE9, al

    sti
    int 0x19


; =============================================================================
; Interrupt Handlers
; =============================================================================
%include "int08_timer.inc"
%include "int09_kbd.inc"
%include "int10_video.inc"

; =============================================================================
; INT 11h — Equipment List
; =============================================================================
int11_handler:
    push ds
    mov ax, 0x0040
    mov ds, ax
    mov ax, [BDA_EQUIPMENT]
    pop ds
    iret


; =============================================================================
; INT 12h — Memory Size
; =============================================================================
int12_handler:
    push ds
    mov ax, 0x0040
    mov ds, ax
    mov ax, [BDA_MEMSIZE]
    pop ds
    iret


%include "int13_disk.inc"

; =============================================================================
; INT 15h — System Services
; =============================================================================
int15_handler:
    cmp ah, 0x88
    je .i15_88
    ; Unsupported function / keyboard intercept passthrough: CF=1
    push bp
    mov bp, sp
    or word [bp+6], 0x0001
    pop bp
    iret

.i15_88:
    ; Extended memory size (none on 8086)
    xor ax, ax
    push bp
    mov bp, sp
    and word [bp+6], 0xFFFE
    pop bp
    iret


%include "int16_kbd.inc"
%include "int19_boot.inc"
%include "int1a_time.inc"
%include "utils.inc"


; =============================================================================
; Floppy Disk Parameter Table (INT 1Eh)
; =============================================================================
floppy_dpt:
    db 0xDF   ; Step rate / head unload (SRT=13, HUT=15)
    db 0x02   ; Head load time / DMA mode
    db 0x25   ; Motor off delay (ticks)
    db 0x02   ; Bytes per sector (0=128, 1=256, 2=512)
    db 18     ; Sectors per track (1.44M default)
    db 0x1B   ; Gap length
    db 0xFF   ; Data length
    db 0x54   ; Format gap length
    db 0xF6   ; Format fill byte
    db 0x0F   ; Head settle time (ms)
    db 0x08   ; Motor start time (1/8 seconds)


; =============================================================================
; Pad + reset vector + BIOS identification
; =============================================================================
times (0xFFF0 - ($ - $$)) db 0xFF

reset_vector:
    jmp 0xF000:bios_entry       ; Reset vector at F000:FFF0 (5 bytes)

    db '02/24/26'               ; BIOS date at F000:FFF5 (8 bytes)
    db 0xFF                     ; Submodel byte at F000:FFFD
    db 0xFE                     ; Model byte at F000:FFFE (0xFE = IBM PC/XT)
    db 0x00                     ; Checksum at F000:FFFF
