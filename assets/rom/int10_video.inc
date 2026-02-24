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
    cmp ah, 0x0B
    je .set_palette
    cmp ah, 0x0E
    je .teletype
    cmp ah, 0x0F
    je .get_mode
    cmp ah, 0x13
    je .write_string
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
    mov byte [BDA_ACTIVE_PAGE], 0
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
    push si
    mov ax, 0x0040
    mov ds, ax
    ; Store cursor for the specified page (BH)
    mov al, bh
    xor ah, ah
    shl ax, 1
    mov si, ax
    mov [BDA_CURSOR_POS + si], dx
    ; Only update CRTC if this is the active page
    cmp bh, [BDA_ACTIVE_PAGE]
    jne .scp_done
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
.scp_done:
    pop si
    pop dx
    pop bx
    pop ax
    pop ds
    iret

; --- AH=03: Get cursor position ---
.get_cursor_pos:
    push ds
    push ax
    push si
    mov ax, 0x0040
    mov ds, ax
    mov al, bh
    xor ah, ah
    shl ax, 1
    mov si, ax
    mov dx, [BDA_CURSOR_POS + si]
    mov cx, [BDA_CURSOR_SHAPE]
    pop si
    pop ax
    pop ds
    iret

; --- AH=05: Set active display page ---
.set_active_page:
    push ds
    push bx
    mov bx, 0x0040
    mov ds, bx
    mov [BDA_ACTIVE_PAGE], al
    pop bx
    pop ds
    iret

; --- AH=06: Scroll up ---
; AL = lines (0=clear window), BH = fill attribute
; CH/CL = top-left row/col, DH/DL = bottom-right row/col
.scroll_up:
    push ds
    push es
    push si
    push di
    push ax
    push bx
    push cx
    push dx
    push bp
    mov bp, sp
    ; [bp+8]=AX [bp+6]=BX [bp+4]=CX [bp+2]=DX

    mov ax, 0xB800
    mov ds, ax
    mov es, ax

    ; Window height
    mov al, [bp+3]          ; DH (bottom row)
    sub al, [bp+5]          ; - CH (top row)
    inc al                  ; AL = window height
    mov ah, [bp+8]          ; original AL = scroll count
    cmp ah, 0
    je .su_do_clear
    cmp ah, al
    jae .su_do_clear

    ; --- Copy rows upward ---
    sub al, ah              ; AL = rows to copy
    xor ch, ch
    mov cl, al              ; CX = row counter

    ; Width in columns
    mov al, [bp+2]          ; DL (right col)
    sub al, [bp+4]          ; - CL (left col)
    inc al
    xor ah, ah
    push ax                 ; save width

    ; Dest offset = (top_row * 80 + left_col) * 2
    mov al, [bp+5]          ; top row
    xor ah, ah
    mov bl, 80
    mul bl
    mov bl, [bp+4]          ; left col
    xor bh, bh
    add ax, bx
    shl ax, 1
    mov di, ax

    ; Source offset = ((top_row + scroll_count) * 80 + left_col) * 2
    mov al, [bp+5]
    add al, [bp+8]          ; + scroll count
    xor ah, ah
    mov bl, 80
    mul bl
    mov bl, [bp+4]
    xor bh, bh
    add ax, bx
    shl ax, 1
    mov si, ax

    pop bx                  ; BX = width
    cld
.su_copy_row:
    push cx
    mov cx, bx
    push si
    push di
    rep movsw
    pop di
    pop si
    add si, 160
    add di, 160
    pop cx
    loop .su_copy_row

    ; Clear bottom rows (DI already at first blank row)
    mov cl, [bp+8]          ; scroll count
    xor ch, ch
    mov al, 0x20
    mov ah, [bp+7]          ; fill attribute
.su_fill_row:
    push cx
    mov cx, bx
    push di
    rep stosw
    pop di
    add di, 160
    pop cx
    loop .su_fill_row
    jmp .su_done

.su_do_clear:
    ; Clear entire window
    mov al, [bp+5]          ; top row
    xor ah, ah
    mov bl, 80
    mul bl
    mov bl, [bp+4]          ; left col
    xor bh, bh
    add ax, bx
    shl ax, 1
    mov di, ax

    mov al, [bp+2]          ; right col
    sub al, [bp+4]          ; - left col
    inc al
    xor ah, ah
    mov bx, ax              ; BX = width

    mov al, [bp+3]          ; bottom row
    sub al, [bp+5]          ; - top row
    inc al
    xor ah, ah
    mov cx, ax              ; CX = height

    mov al, 0x20
    mov ah, [bp+7]          ; fill attribute
    cld
.su_clear_row:
    push cx
    mov cx, bx
    push di
    rep stosw
    pop di
    add di, 160
    pop cx
    loop .su_clear_row

.su_done:
    pop bp
    pop dx
    pop cx
    pop bx
    pop ax
    pop di
    pop si
    pop es
    pop ds
    iret

; --- AH=07: Scroll down ---
; AL = lines (0=clear window), BH = fill attribute
; CH/CL = top-left row/col, DH/DL = bottom-right row/col
.scroll_down:
    push ds
    push es
    push si
    push di
    push ax
    push bx
    push cx
    push dx
    push bp
    mov bp, sp

    mov ax, 0xB800
    mov ds, ax
    mov es, ax

    ; Window height
    mov al, [bp+3]          ; DH (bottom row)
    sub al, [bp+5]          ; - CH (top row)
    inc al
    mov ah, [bp+8]          ; original AL = scroll count
    cmp ah, 0
    je .sd_do_clear
    cmp ah, al
    jae .sd_do_clear

    ; --- Copy rows downward ---
    sub al, ah              ; AL = rows to copy
    xor ch, ch
    mov cl, al              ; CX = row counter

    ; Width in columns
    mov al, [bp+2]          ; DL (right col)
    sub al, [bp+4]          ; - CL (left col)
    inc al
    xor ah, ah
    push ax                 ; save width

    ; Dest offset = (bottom_row * 80 + left_col) * 2
    mov al, [bp+3]          ; bottom row
    xor ah, ah
    mov bl, 80
    mul bl
    mov bl, [bp+4]
    xor bh, bh
    add ax, bx
    shl ax, 1
    mov di, ax

    ; Source offset = ((bottom_row - scroll_count) * 80 + left_col) * 2
    mov al, [bp+3]
    sub al, [bp+8]          ; - scroll count
    xor ah, ah
    mov bl, 80
    mul bl
    mov bl, [bp+4]
    xor bh, bh
    add ax, bx
    shl ax, 1
    mov si, ax

    pop bx                  ; BX = width
    cld
.sd_copy_row:
    push cx
    mov cx, bx
    push si
    push di
    rep movsw
    pop di
    pop si
    sub si, 160             ; move up one row
    sub di, 160
    pop cx
    loop .sd_copy_row

    ; Clear top rows
    mov al, [bp+5]          ; top row
    xor ah, ah
    push bx
    mov bl, 80
    mul bl
    pop bx
    mov bl, [bp+4]          ; left col
    xor bh, bh
    add ax, bx
    shl ax, 1
    mov di, ax

    ; Recover width
    mov al, [bp+2]
    sub al, [bp+4]
    inc al
    xor ah, ah
    mov bx, ax              ; BX = width

    mov cl, [bp+8]          ; scroll count
    xor ch, ch
    mov al, 0x20
    mov ah, [bp+7]          ; fill attribute
.sd_fill_row:
    push cx
    mov cx, bx
    push di
    rep stosw
    pop di
    add di, 160
    pop cx
    loop .sd_fill_row
    jmp .sd_done

.sd_do_clear:
    ; Clear entire window (same as scroll up clear)
    mov al, [bp+5]
    xor ah, ah
    mov bl, 80
    mul bl
    mov bl, [bp+4]
    xor bh, bh
    add ax, bx
    shl ax, 1
    mov di, ax

    mov al, [bp+2]
    sub al, [bp+4]
    inc al
    xor ah, ah
    mov bx, ax

    mov al, [bp+3]
    sub al, [bp+5]
    inc al
    xor ah, ah
    mov cx, ax

    mov al, 0x20
    mov ah, [bp+7]
    cld
.sd_clear_row:
    push cx
    mov cx, bx
    push di
    rep stosw
    pop di
    add di, 160
    pop cx
    loop .sd_clear_row

.sd_done:
    pop bp
    pop dx
    pop cx
    pop bx
    pop ax
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
    mov al, byte [BDA_CURSOR_POS+1]
    mov bl, 80
    mul bl
    xor bh, bh
    mov bl, byte [BDA_CURSOR_POS]
    add ax, bx
    mov si, ax
    add si, si
    mov bx, 0xB800
    mov ds, bx
    lodsw
    mov bx, ax
    mov al, bl
    mov ah, bh
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
    mov ah, bl
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

; --- AH=0B: Set color palette (text mode no-op) ---
.set_palette:
    iret

; --- AH=0E: Teletype output ---
.teletype:
    push ds
    push es
    push bx
    push cx
    push dx
    push di

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
    mov [es:di], al

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
.get_mode:
    push ds
    mov bx, 0x0040
    mov ds, bx
    mov al, [BDA_VIDEO_MODE]
    mov ah, byte [BDA_VIDEO_COLS]
    mov bh, [BDA_ACTIVE_PAGE]
    pop ds
    iret

; --- AH=13: Write string ---
; AL = write mode (bit 0: move cursor, bit 1: string has attr bytes)
; BH = page (ignored), BL = attribute (if AL bit 1 = 0)
; CX = string length
; DH = row, DL = col
; ES:BP = pointer to string
;
; Per IBM BIOS: control chars (07h, 08h, 0Ah, 0Dh) are processed as
; commands, not written. Printable chars are written with the attribute.
.write_string:
    push ds
    push si
    push di
    push dx
    push cx
    push bx
    push ax

    ; Set cursor to DH,DL
    push ax
    push bx
    mov ah, 0x02
    xor bh, bh
    int 0x10
    pop bx
    pop ax

    ; SI = string offset, DS = string segment
    mov si, bp
    push es
    pop ds

    mov di, cx          ; DI = remaining char count
    test al, 0x02
    jnz .ws_with_attr

    ; --- Char-only mode (attribute in BL) ---
.ws_char_only:
    cmp di, 0
    je .ws_done
    lodsb
    ; Check for control characters
    cmp al, 0x07
    je .ws_co_ctrl
    cmp al, 0x08
    je .ws_co_ctrl
    cmp al, 0x0A
    je .ws_co_ctrl
    cmp al, 0x0D
    je .ws_co_ctrl
    ; Printable: write char+attr then advance via teletype
    push di
    push bx
    mov ah, 0x09
    mov cx, 1
    xor bh, bh
    int 0x10
    pop bx
    pop di
.ws_co_ctrl:
    ; Advance cursor / process control char via teletype
    push di
    push bx
    mov ah, 0x0E
    xor bh, bh
    int 0x10
    pop bx
    pop di
    dec di
    jmp .ws_char_only

    ; --- With-attr mode (each char followed by attr byte in string) ---
.ws_with_attr:
    cmp di, 0
    je .ws_done
    lodsb               ; AL = character
    mov bl, [si]        ; BL = attribute from string
    inc si
    ; Check for control characters
    cmp al, 0x07
    je .ws_wa_ctrl
    cmp al, 0x08
    je .ws_wa_ctrl
    cmp al, 0x0A
    je .ws_wa_ctrl
    cmp al, 0x0D
    je .ws_wa_ctrl
    ; Printable: write char+attr then advance via teletype
    push di
    push bx
    mov ah, 0x09
    mov cx, 1
    xor bh, bh
    int 0x10
    pop bx
    pop di
.ws_wa_ctrl:
    ; Advance cursor / process control char via teletype
    push di
    push bx
    mov ah, 0x0E
    xor bh, bh
    int 0x10
    pop bx
    pop di
    dec di
    jmp .ws_with_attr

.ws_done:
    pop ax
    test al, 0x01
    jnz .ws_keep_cursor
    pop bx
    pop cx
    pop dx
    mov ah, 0x02
    xor bh, bh
    int 0x10
    pop di
    pop si
    pop ds
    iret

.ws_keep_cursor:
    pop bx
    pop cx
    pop dx
    pop di
    pop si
    pop ds
    iret
