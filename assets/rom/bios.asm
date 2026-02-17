; =============================================================================
; 8086 BIOS ROM — Full handlers
; Assemble: nasm -f bin bios.asm -o bios.bin
; =============================================================================

cpu 8086
bits 16
org 0x0000

; --- BDA offsets (segment 0x0040) ---
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
BDA_KBD_BUF_START equ 0x0080
BDA_KBD_BUF_END   equ 0x0082


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
; INT 08h — Timer Tick (IRQ0)
; =============================================================================
int08_handler:
    push ds
    push ax
    mov ax, 0x0040
    mov ds, ax
    inc word [BDA_TIMER_LOW]
    jnz .no_hi
    inc word [BDA_TIMER_HIGH]
.no_hi:
    cmp word [BDA_TIMER_HIGH], 0x0018
    jb .no_roll
    cmp word [BDA_TIMER_LOW], 0x00B0
    jb .no_roll
    mov word [BDA_TIMER_HIGH], 0
    mov word [BDA_TIMER_LOW], 0
    mov byte [BDA_TIMER_OFLOW], 1
.no_roll:
    mov al, 0x20
    out 0x20, al
    pop ax
    pop ds
    iret


; =============================================================================
; INT 09h — Keyboard (IRQ1)
; =============================================================================
int09_handler:
    push ax
    push bx
    push si
    push ds

    mov ax, 0x0040
    mov ds, ax

    in al, 0x60

    ; ACK keyboard
    xchg al, ah       ; scancode moves to AH, frees AL
    in al, 0x61
    or al, 0x80
    out 0x61, al
    and al, 0x7F
    out 0x61, al
    xchg al, ah       ; scancode back in AL

    ; Ignore break codes
    test al, 0x80
    jnz .k_done

    ; Translate
    mov ah, al
    xor bx, bx
    mov bl, al
    cmp bl, scancode_table_end - scancode_table
    jae .k_done

    mov si, scancode_table
    cs mov al, [si+bx]
    cmp al, 0
    je .k_done

    ; Insert into buffer
    mov bx, [BDA_KBD_BUF_TAIL]
    mov si, bx
    add si, 2
    cmp si, [BDA_KBD_BUF_END]
    jb .k_nw
    mov si, [BDA_KBD_BUF_START]
.k_nw:
    cmp si, [BDA_KBD_BUF_HEAD]
    je .k_done
    mov [bx], ax
    mov [BDA_KBD_BUF_TAIL], si

.k_done:
    mov al, 0x20
    out 0x20, al
    pop ds
    pop si
    pop bx
    pop ax
    iret

scancode_table:
    db 0, 0x1B                                      ; 0x00-0x01
    db '1','2','3','4','5','6','7','8','9','0'       ; 0x02-0x0B
    db '-','='                                       ; 0x0C-0x0D
    db 0x08, 0x09                                    ; 0x0E-0x0F
    db 'q','w','e','r','t','y','u','i','o','p'       ; 0x10-0x19
    db '[',']'                                       ; 0x1A-0x1B
    db 0x0D, 0                                       ; 0x1C-0x1D
    db 'a','s','d','f','g','h','j','k','l'           ; 0x1E-0x26
    db ';', 0x27, '`'                                ; 0x27-0x29
    db 0, 0x5C                                       ; 0x2A-0x2B
    db 'z','x','c','v','b','n','m'                   ; 0x2C-0x32
    db ',','.','/'                                   ; 0x33-0x35
    db 0, '*', 0, ' '                                ; 0x36-0x39
scancode_table_end:


; =============================================================================
; INT 10h — Video Services
; =============================================================================
int10_handler:
    cmp ah, 0x00
    je .set_mode
    cmp ah, 0x01
    je .set_cursor_shape
    cmp ah, 0x02
    je .set_cursor_pos
    cmp ah, 0x03
    je .get_cursor_pos
    cmp ah, 0x05
    je .set_active_page
    cmp ah, 0x06
    je .scroll_up
    cmp ah, 0x07
    je .scroll_down
    cmp ah, 0x08
    je .read_char_attr
    cmp ah, 0x09
    je .write_char_attr
    cmp ah, 0x0A
    je .write_char
    cmp ah, 0x0E
    je .teletype
    cmp ah, 0x0F
    je .get_mode
    iret

; --- AH=00: Set video mode ---
.set_mode:
    push ds
    push es
    push di
    push cx
    push ax
    mov bx, 0x0040
    mov ds, bx
    mov byte [BDA_VIDEO_MODE], al
    mov word [BDA_VIDEO_COLS], 80
    mov word [BDA_CURSOR_POS], 0x0000
    mov bx, 0xB800
    mov es, bx
    xor di, di
    mov cx, 2000
    mov ax, 0x0720
    cld
    rep stosw
    pop ax
    pop cx
    pop di
    pop es
    pop ds
    iret

; --- AH=01: Set cursor shape ---
.set_cursor_shape:
    push ds
    push ax
    mov ax, 0x0040
    mov ds, ax
    mov [BDA_CURSOR_SHAPE], cx
    pop ax
    pop ds
    iret

; --- AH=02: Set cursor position ---
.set_cursor_pos:
    push ds
    push ax
    push bx
    push dx
    mov ax, 0x0040
    mov ds, ax
    mov [BDA_CURSOR_POS], dx
    ; Update CRTC
    mov al, dh
    xor ah, ah
    mov bl, 80
    mul bl
    xor dh, dh
    add ax, dx
    mov bx, ax
    mov dx, 0x03D4
    mov al, 0x0E
    out dx, al
    mov dx, 0x03D5
    mov al, bh
    out dx, al
    mov dx, 0x03D4
    mov al, 0x0F
    out dx, al
    mov dx, 0x03D5
    mov al, bl
    out dx, al
    pop dx
    pop bx
    pop ax
    pop ds
    iret

; --- AH=03: Get cursor position ---
.get_cursor_pos:
    push ds
    push ax
    mov ax, 0x0040
    mov ds, ax
    mov dx, [BDA_CURSOR_POS]
    mov cx, [BDA_CURSOR_SHAPE]
    pop ax
    pop ds
    iret

; --- AH=05: Set active display page ---
.set_active_page:
    push ds
    push ax
    mov ax, 0x0040
    mov ds, ax
    mov [BDA_ACTIVE_PAGE], al
    pop ax
    pop ds
    iret

; --- AH=06: Scroll up ---
.scroll_up:
    cmp al, 0
    jne .scroll_up_lines
    ; AL=0: clear window
    push es
    push di
    push cx
    push ax
    mov ax, 0xB800
    mov es, ax
    xor di, di
    mov cx, 2000
    mov al, ' '
    mov ah, bh
    cld
    rep stosw
    pop ax
    pop cx
    pop di
    pop es
    iret
.scroll_up_lines:
    push ds
    push es
    push si
    push di
    push cx
    push ax
    push dx
    mov cl, al
    xor ch, ch
    mov ax, 0xB800
    mov ds, ax
    mov es, ax
    mov ax, 160
    mul cx
    mov si, ax
    xor di, di
    push cx
    mov al, 25
    sub al, cl
    xor ah, ah
    mov bl, 80
    mul bl
    mov cx, ax
    cld
    rep movsw
    pop cx
    mov al, cl
    xor ah, ah
    mov bl, 80
    mul bl
    mov cx, ax
    mov ax, 0x0720
    rep stosw
    pop dx
    pop ax
    pop cx
    pop di
    pop si
    pop es
    pop ds
    iret

; --- AH=07: Scroll down ---
.scroll_down:
    cmp al, 0
    jne .scroll_down_lines
    push es
    push di
    push cx
    push ax
    mov ax, 0xB800
    mov es, ax
    xor di, di
    mov cx, 2000
    mov al, ' '
    mov ah, bh
    cld
    rep stosw
    pop ax
    pop cx
    pop di
    pop es
    iret
.scroll_down_lines:
    push ds
    push es
    push si
    push di
    push cx
    push ax
    push dx
    mov cl, al
    xor ch, ch
    mov ax, 0xB800
    mov ds, ax
    mov es, ax
    ; Source: last data row, dest: last screen row
    ; Work backwards
    mov ax, 24
    mov bl, 80
    mul bl
    mov di, ax
    add di, di              ; DI = byte offset of row 24
    mov al, 24
    sub al, cl
    mul bl
    mov si, ax
    add si, si              ; SI = byte offset of last source row
    push cx
    mov al, 25
    sub al, cl
    xor ah, ah
    mov bl, 80
    mul bl
    mov cx, ax              ; words to move
    std
    add si, 158             ; point to end of source row
    add di, 158             ; point to end of dest row
    rep movsw
    cld
    pop cx
    ; Fill top lines with blanks
    xor di, di
    mov al, cl
    xor ah, ah
    mov bl, 80
    mul bl
    mov cx, ax
    mov ax, 0x0720
    rep stosw
    pop dx
    pop ax
    pop cx
    pop di
    pop si
    pop es
    pop ds
    iret

; --- AH=08: Read char+attr at cursor ---
.read_char_attr:
    push ds
    push si
    push bx
    mov bx, 0x0040
    mov ds, bx
    mov bx, 0xB800
    mov ds, bx
    ; Calculate offset from BDA cursor pos
    push ds
    mov bx, 0x0040
    mov ds, bx
    mov al, byte [BDA_CURSOR_POS+1] ; row
    mov bl, 80
    mul bl
    xor bh, bh
    mov bl, byte [BDA_CURSOR_POS]   ; col
    add ax, bx
    mov si, ax
    add si, si
    pop ds
    ; DS still = 0xB800? No, we changed it. Fix:
    mov bx, 0xB800
    mov ds, bx
    lodsw                   ; AL=char, AH=attr
    ; AH = attribute, AL = character
    mov bx, ax
    mov al, bl              ; AL = char
    mov ah, bh              ; AH = attr
    pop bx
    pop si
    pop ds
    iret

; --- AH=09: Write char+attr at cursor ---
.write_char_attr:
    push es
    push di
    push dx
    push ds
    push cx
    push ax
    mov dx, 0x0040
    mov ds, dx
    mov dh, byte [BDA_CURSOR_POS+1]
    mov dl, byte [BDA_CURSOR_POS]
    pop ax
    push ax
    mov al, dh
    xor ah, ah
    mov di, 80
    push dx
    mul di
    pop dx
    xor dh, dh
    add ax, dx
    mov di, ax
    add di, di
    pop ax
    mov dx, 0xB800
    mov es, dx
    mov ah, bl              ; attribute from BL
.wa_loop:
    mov [es:di], ax
    add di, 2
    loop .wa_loop
    pop cx
    pop ds
    pop dx
    pop di
    pop es
    iret

; --- AH=0A: Write char at cursor (keep attr) ---
.write_char:
    push es
    push di
    push dx
    push ds
    push cx
    push ax
    mov dx, 0x0040
    mov ds, dx
    mov dh, byte [BDA_CURSOR_POS+1]
    mov dl, byte [BDA_CURSOR_POS]
    pop ax
    push ax
    push dx
    mov al, dh
    xor ah, ah
    mov di, 80
    mul di
    pop dx
    xor dh, dh
    add ax, dx
    mov di, ax
    add di, di
    pop ax
    mov dx, 0xB800
    mov es, dx
.wc_loop:
    mov [es:di], al
    add di, 2
    loop .wc_loop
    pop cx
    pop ds
    pop dx
    pop di
    pop es
    iret

; --- AH=0E: Teletype output ---
.teletype:
    push ds
    push es
    push bx
    push cx
    push dx
    push di

    ; Also send to debug port
    out 0xE9, al

    mov bx, 0x0040
    mov ds, bx
    mov dh, byte [BDA_CURSOR_POS+1]
    mov dl, byte [BDA_CURSOR_POS]

    cmp al, 0x0D
    je .t_cr
    cmp al, 0x0A
    je .t_lf
    cmp al, 0x08
    je .t_bs
    cmp al, 0x07
    je .t_done

    ; Write character to video RAM
    push ax
    mov al, dh
    xor ah, ah
    mov bx, 80
    push dx
    mul bx
    pop dx
    xor dh, dh
    add ax, dx
    mov di, ax
    add di, di
    pop ax

    mov bx, 0xB800
    mov es, bx
    mov ah, 0x07
    mov [es:di], ax

    ; Advance cursor
    mov dh, byte [BDA_CURSOR_POS+1]
    mov dl, byte [BDA_CURSOR_POS]
    inc dl
    cmp dl, 80
    jb .t_update
    xor dl, dl
    jmp .t_newline

.t_cr:
    xor dl, dl
    jmp .t_update

.t_lf:
.t_newline:
    inc dh
    cmp dh, 25
    jb .t_update
    mov dh, 24
    ; Scroll up one line
    push ax
    push si
    push di
    push cx
    mov ax, 0xB800
    mov ds, ax
    mov es, ax
    mov si, 160
    xor di, di
    mov cx, 1920
    cld
    rep movsw
    mov cx, 80
    mov ax, 0x0720
    rep stosw
    mov ax, 0x0040
    mov ds, ax
    pop cx
    pop di
    pop si
    pop ax
    jmp .t_update

.t_bs:
    cmp dl, 0
    je .t_update
    dec dl
    jmp .t_update

.t_update:
    mov [BDA_CURSOR_POS], dl
    mov [BDA_CURSOR_POS+1], dh
    ; Update CRTC cursor
    push ax
    push bx
    mov al, dh
    xor ah, ah
    mov bl, 80
    mul bl
    xor dh, dh
    add ax, dx
    mov bx, ax
    mov dx, 0x03D4
    mov al, 0x0E
    out dx, al
    mov dx, 0x03D5
    mov al, bh
    out dx, al
    mov dx, 0x03D4
    mov al, 0x0F
    out dx, al
    mov dx, 0x03D5
    mov al, bl
    out dx, al
    pop bx
    pop ax

.t_done:
    pop di
    pop dx
    pop cx
    pop bx
    pop es
    pop ds
    iret

; --- AH=0F: Get video mode ---
.v_get_mode:
    push ds
    mov bx, 0x0040
    mov ds, bx
    mov al, [BDA_VIDEO_MODE]
    mov ah, byte [BDA_VIDEO_COLS]
    mov bh, [BDA_ACTIVE_PAGE]
    pop ds
    iret

.get_mode:
    jmp .v_get_mode


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


; =============================================================================
; INT 13h — Disk (emulator trap)
; =============================================================================
int13_handler:
    out 0xB0, al
    retf 2


; =============================================================================
; INT 15h — System Services (stub)
; =============================================================================
int15_handler:
    stc
    iret


; =============================================================================
; INT 16h — Keyboard Services
; =============================================================================
int16_handler:
    cmp ah, 0x00
    je .kb_read
    cmp ah, 0x01
    je .kb_status
    cmp ah, 0x02
    je .kb_flags
    iret

.kb_read:
    sti
    push ds
    push bx
    mov bx, 0x0040
    mov ds, bx
.kb_wait:
    cli
    mov bx, [BDA_KBD_BUF_HEAD]
    cmp bx, [BDA_KBD_BUF_TAIL]
    jne .kb_got
    sti
    hlt
    jmp .kb_wait
.kb_got:
    mov ax, [bx]
    add bx, 2
    cmp bx, [BDA_KBD_BUF_END]
    jb .kb_nw
    mov bx, [BDA_KBD_BUF_START]
.kb_nw:
    mov [BDA_KBD_BUF_HEAD], bx
    sti
    pop bx
    pop ds
    iret

.kb_status:
    push ds
    push bx
    mov bx, 0x0040
    mov ds, bx
    cli
    mov bx, [BDA_KBD_BUF_HEAD]
    cmp bx, [BDA_KBD_BUF_TAIL]
    je .kb_empty
    mov ax, [bx]
    pop bx
    pop ds
    ; Clear ZF to indicate key available
    push bp
    mov bp, sp
    and word [bp+6], 0xFFBF
    pop bp
    sti
    iret
.kb_empty:
    pop bx
    pop ds
    ; Set ZF to indicate no key
    push bp
    mov bp, sp
    or word [bp+6], 0x0040
    pop bp
    sti
    iret

.kb_flags:
    push ds
    mov bx, 0x0040
    mov ds, bx
    mov al, [BDA_KBD_FLAGS1]
    pop ds
    iret


; =============================================================================
; INT 19h — Bootstrap
; =============================================================================
int19_handler:
    mov al, 'L'
    out 0xE9, al

    mov di, boot_order
    mov cl, boot_order_len

.try_next:
    cmp cl, 0
    je .fail

    cs mov dl, [di]
    inc di
    dec cl

    mov ax, 0x0000
    mov es, ax
    mov bx, 0x7C00
    mov ah, 0x02
    mov al, 0x01
    mov ch, 0x00
    mov cl, 0x01
    mov dh, 0x00
    int 0x13
    jc .try_next

    cmp word [es:0x7DFE], 0xAA55
    jne .try_next

    mov al, '!'
    out 0xE9, al
    cs mov dl, [di-1]
    jmp 0x0000:0x7C00

.fail:
    mov al, 'F'
    out 0xE9, al
    mov si, msg_fail
    call print_cs
.halt:
    hlt
    jmp .halt

boot_order:
    db 0x00                 ; Floppy A:
    db 0x80                 ; Hard disk 0
boot_order_len equ $ - boot_order


; =============================================================================
; INT 1Ah — Time of Day
; =============================================================================
int1a_handler:
    cmp ah, 0x00
    je .tod_read
    cmp ah, 0x01
    je .tod_write
    iret

.tod_read:
    push ds
    mov ax, 0x0040
    mov ds, ax
    mov cx, [BDA_TIMER_HIGH]
    mov dx, [BDA_TIMER_LOW]
    mov al, [BDA_TIMER_OFLOW]
    mov byte [BDA_TIMER_OFLOW], 0
    pop ds
    iret

.tod_write:
    push ds
    push ax
    mov ax, 0x0040
    mov ds, ax
    mov [BDA_TIMER_HIGH], cx
    mov [BDA_TIMER_LOW], dx
    mov byte [BDA_TIMER_OFLOW], 0
    pop ax
    pop ds
    iret


; =============================================================================
; Utilities
; =============================================================================
print_cs:
    push ax
    push bx
.ps_loop:
    cs lodsb
    cmp al, 0
    je .ps_done
    mov ah, 0x0E
    xor bx, bx
    int 0x10
    jmp .ps_loop
.ps_done:
    pop bx
    pop ax
    ret

iret_stub:
    iret

msg_fail:
    db 'No bootable disk', 0x0D, 0x0A, 0


; =============================================================================
; Pad + reset vector
; =============================================================================
times (0xFFF0 - ($ - $$)) db 0xFF

reset_vector:
    jmp 0xF000:bios_entry

times (0x10000 - ($ - $$)) db 0xFF