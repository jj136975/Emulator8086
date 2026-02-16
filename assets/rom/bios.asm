; =============================================================================
; Minimal 8086 BIOS - Skeleton for emulator bring-up
; Assemble: nasm -f bin bios.asm -o bios.bin
;
; Tests: CPU reset vector -> IVT init -> boot sector load -> jump to 0000:7C00
;
; Magic emulator ports:
;   0xE9 - Debug console (write byte -> emulator prints to stdout)
;   0xB0 - Disk trap     (write -> emulator does disk I/O using CPU regs)
; =============================================================================

cpu 8086
bits 16
org 0x0000                  ; mapped at F000:0000 (physical 0xF0000)

; --- BDA offsets (segment 0x0040) ---
BDA_MEMSIZE       equ 0x0013
BDA_KBD_BUF_HEAD  equ 0x001A
BDA_KBD_BUF_TAIL  equ 0x001C
BDA_VIDEO_MODE    equ 0x0049
BDA_VIDEO_COLS    equ 0x004A
BDA_CURSOR_POS    equ 0x0050
BDA_ACTIVE_PAGE   equ 0x0062
BDA_TIMER_LOW     equ 0x006C
BDA_TIMER_HIGH    equ 0x006E
BDA_TIMER_OFLOW   equ 0x0070
BDA_KBD_BUF_START equ 0x0080
BDA_KBD_BUF_END   equ 0x0082


; =============================================================================
; BIOS ENTRY - jumped to from reset vector at the bottom
; =============================================================================
bios_entry:
    cli

    ; Stack below boot sector area
    xor ax, ax
    mov ss, ax
    mov sp, 0x7C00

    ; Debug: we are alive
    mov al, 'B'
    out 0xE9, al

    ; Zero IVT (0x000-0x3FF) + BDA (0x400-0x4FF)
    xor ax, ax
    mov es, ax
    xor di, di
    mov cx, 0x0280          ; 0x500 bytes as words
    cld
    rep stosw

    ; --- Install IVT entries (all in segment F000) ---
    xor ax, ax
    mov ds, ax

    mov word [0x0020], int08_handler    ; IRQ0 timer
    mov word [0x0022], 0xF000
    mov word [0x0024], int09_handler    ; IRQ1 keyboard
    mov word [0x0026], 0xF000
    mov word [0x0040], int10_handler    ; video
    mov word [0x0042], 0xF000
    mov word [0x0044], int11_handler    ; equipment
    mov word [0x0046], 0xF000
    mov word [0x0048], int12_handler    ; memory size
    mov word [0x004A], 0xF000
    mov word [0x004C], int13_handler    ; disk
    mov word [0x004E], 0xF000
    mov word [0x0058], int16_handler    ; keyboard read
    mov word [0x005A], 0xF000
    mov word [0x0064], int19_handler    ; bootstrap
    mov word [0x0066], 0xF000
    mov word [0x0068], int1a_handler    ; time of day
    mov word [0x006A], 0xF000

    ; Fill remaining IVT slots with IRET stub
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

    ; --- Initialize BDA ---
    mov ax, 0x0040
    mov ds, ax
    mov word [BDA_MEMSIZE], 640
    mov byte [BDA_VIDEO_MODE], 0x03
    mov word [BDA_VIDEO_COLS], 80
    mov word [BDA_CURSOR_POS], 0x0000
    mov byte [BDA_ACTIVE_PAGE], 0
    mov word [BDA_TIMER_LOW], 0
    mov word [BDA_TIMER_HIGH], 0
    mov byte [BDA_TIMER_OFLOW], 0
    mov word [BDA_KBD_BUF_HEAD], 0x001E
    mov word [BDA_KBD_BUF_TAIL], 0x001E
    mov word [BDA_KBD_BUF_START], 0x001E
    mov word [BDA_KBD_BUF_END], 0x003E

    ; --- Init PIC (minimal) ---
    mov al, 0x11
    out 0x20, al            ; ICW1
    mov al, 0x08
    out 0x21, al            ; ICW2: IRQ0 = INT 08h
    mov al, 0x04
    out 0x21, al            ; ICW3
    mov al, 0x01
    out 0x21, al            ; ICW4: 8086 mode
    mov al, 0xFC
    out 0x21, al            ; unmask IRQ0 + IRQ1

    ; --- Init PIT channel 0 ---
    mov al, 0x36
    out 0x43, al            ; ch0, mode 3, lo/hi
    mov al, 0x00
    out 0x40, al            ; divisor = 65536 (~18.2 Hz)
    out 0x40, al

    ; Debug: init done
    mov al, 'I'
    out 0xE9, al

    ; Boot
    sti
    int 0x19


; =============================================================================
; INT 08h - Timer Tick (IRQ0) - stub
; =============================================================================
int08_handler:
    push ds
    push ax
    mov ax, 0x0040
    mov ds, ax
    inc word [BDA_TIMER_LOW]
    jnz .done
    inc word [BDA_TIMER_HIGH]
.done:
    mov al, 0x20
    out 0x20, al            ; EOI
    pop ax
    pop ds
    iret


; =============================================================================
; INT 09h - Keyboard (IRQ1) - stub
; Reads scancode to clear IRQ, sends EOI, discards input.
; =============================================================================
int09_handler:
    push ax
    in al, 0x60
    mov al, 0x20
    out 0x20, al
    pop ax
    iret


; =============================================================================
; INT 10h - Video - stub
; Only AH=0Eh (teletype) works, routed to debug port 0xE9.
; =============================================================================
int10_handler:
    cmp ah, 0x0E
    je .tty
    cmp ah, 0x03
    je .get_cursor
    cmp ah, 0x0F
    je .get_mode
    iret

.tty:
    out 0xE9, al
    iret

.get_cursor:
    push ds
    push ax
    mov ax, 0x0040
    mov ds, ax
    mov dx, [BDA_CURSOR_POS]
    pop ax
    pop ds
    mov cx, 0x0607
    iret

.get_mode:
    mov al, 0x03
    mov ah, 80
    mov bh, 0
    iret


; =============================================================================
; INT 11h - Equipment List - stub
; =============================================================================
int11_handler:
    mov ax, 0x0021          ; floppy + CGA 80x25
    iret


; =============================================================================
; INT 12h - Memory Size - stub
; =============================================================================
int12_handler:
    mov ax, 640
    iret


; =============================================================================
; INT 13h - Disk - emulator trap
; OUT to 0xB0 tells the emulator to read CPU regs, do disk I/O,
; and set return regs + carry flag.
; =============================================================================
int13_handler:
    out 0xB0, al
    iret


; =============================================================================
; INT 16h - Keyboard Read - stub
; =============================================================================
int16_handler:
    cmp ah, 0x00
    je .read
    cmp ah, 0x01
    je .status
    xor al, al
    iret

.read:
    sti
    hlt
    jmp .read               ; spin until keyboard is implemented

.status:
    ; Return ZF=1 (no key)
    push bp
    mov bp, sp
    or word [bp+6], 0x0040
    pop bp
    iret


; =============================================================================
; INT 19h - Bootstrap Loader
; =============================================================================
int19_handler:
    mov al, 'L'
    out 0xE9, al

    ; Read boot sector via INT 13h
    mov ax, 0x0000
    mov es, ax
    mov bx, 0x7C00
    mov ah, 0x02
    mov al, 0x01
    mov ch, 0x00
    mov cl, 0x01
    mov dh, 0x00
    mov dl, 0x00
    int 0x13
    jc .fail

    ; Check signature
    cmp word [es:0x7DFE], 0xAA55
    jne .fail

    mov al, '!'
    out 0xE9, al

    mov dl, 0x00
    jmp 0x0000:0x7C00

.fail:
    mov al, 'F'
    out 0xE9, al
    mov si, msg_fail
    call print_cs
.halt:
    hlt
    jmp .halt


; =============================================================================
; INT 1Ah - Time of Day - stub
; =============================================================================
int1a_handler:
    cmp ah, 0x00
    je .read
    iret
.read:
    push ds
    mov ax, 0x0040
    mov ds, ax
    mov cx, [BDA_TIMER_HIGH]
    mov dx, [BDA_TIMER_LOW]
    mov al, [BDA_TIMER_OFLOW]
    mov byte [BDA_TIMER_OFLOW], 0
    pop ds
    iret


; =============================================================================
; Utilities
; =============================================================================
print_cs:
    push ax
    push bx
.loop:
    cs lodsb
    cmp al, 0
    je .end
    mov ah, 0x0E
    xor bx, bx
    int 0x10
    jmp .loop
.end:
    pop bx
    pop ax
    ret

iret_stub:
    iret

msg_fail:
    db 'No bootable disk', 0x0D, 0x0A, 0


; =============================================================================
; Pad to 64KB, reset vector at 0xFFF0 (physical 0xFFFF0)
; =============================================================================
times (0xFFF0 - ($ - $$)) db 0xFF

reset_vector:
    jmp 0xF000:bios_entry

times (0x10000 - ($ - $$)) db 0xFF