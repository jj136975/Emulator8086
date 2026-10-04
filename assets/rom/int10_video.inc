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
    cmp ah, 0x10
    je .palette
    cmp ah, 0x0C
    je .write_pixel
    cmp ah, 0x0D
    je .read_pixel
    iret

; --- AH=00: Set video mode ---
; AL = mode: 02h/03h (80x25 text) or 13h (320x200 in 256 colours). With bit 7
; set the video memory is not cleared. Any other mode number is ignored, so
; AH=0Fh keeps reporting the mode that is really in effect.
; A mode set starts the display from a known state: page 0, every page's
; cursor at 0,0, the default cursor shape, the CRTC showing video memory from
; its first cell, the mode's default palette, and blank video memory. It
; also fills in the video fields of the BDA (it needs the CRTC base at 40:63).
.set_mode:
    push ds
    push es
    push si
    push di
    push dx
    push cx
    push bx
    push ax
    cld
    mov bx, 0x0040
    mov ds, bx
    mov bh, al                      ; BH bit 7 set: keep the video memory
    mov bl, al
    and bl, 0x7F                    ; BL = mode number
    cmp bl, 0x13
    je .sm_supported
    cmp bl, 0x03
    je .sm_supported
    cmp bl, 0x02
    je .sm_supported
    jmp .sm_done
.sm_supported:
    mov [BDA_VIDEO_MODE], bl

    ; BDA: page 0, all eight cursors at 0,0, default cursor shape
    xor ax, ax
    mov [BDA_ACTIVE_PAGE], al
    mov [BDA_CRT_START], ax
    mov word [BDA_CURSOR_SHAPE], 0x0607
    mov byte [BDA_ROWS_M1], 24
    push ds
    pop es
    mov di, BDA_CURSOR_POS
    mov cx, 8
    rep stosw

    ; CRTC: cursor on scan lines 6-7, display start 0, cursor location 0
    mov dx, [BDA_CRTC_BASE]
    mov si, vid_crtc_reset
    mov cx, 6
.sm_crtc:
    cs lodsw                        ; AL = register, AH = value
    call vid_out_idx
    loop .sm_crtc

    cmp bl, 0x13
    je .sm_13h

    ; --- 80x25 text ---
    mov word [BDA_VIDEO_COLS], 80
    mov word [BDA_CHAR_HEIGHT], 16
    mov dx, 0x03C2                  ; Miscellaneous Output: colour, 400 lines
    mov al, 0x67
    out dx, al
    mov dx, 0x03C4                  ; Sequencer memory mode: odd/even
    mov ax, 0x0204
    call vid_out_idx
    mov dx, 0x03CE                  ; Graphics Controller: text at B8000
    mov ax, 0x1005
    call vid_out_idx
    mov ax, 0x0E06
    call vid_out_idx
    mov dx, 0x03D8                  ; mode control: 80 columns, video on, blink
    mov al, 0x29
    out dx, al
    mov [BDA_CRT_MODE_SET], al
    inc dx                          ; colour select
    mov al, 0x30
    out dx, al
    mov [BDA_CRT_PALETTE], al
    mov si, vid_ac_text             ; attribute controller and DAC
    call vid_load_ac
    call vid_load_dac_ega
    test bh, 0x80
    jnz .sm_done
    mov ax, 0xB800                  ; blank all eight pages
    mov es, ax
    xor di, di
    mov cx, 0x4000
    mov ax, 0x0720
    rep stosw
    jmp .sm_done

.sm_13h:
    ; --- Mode 13h: 320x200x256 (chain-4, linear at A000:0000) ---
    mov word [BDA_VIDEO_COLS], 40
    mov word [BDA_CHAR_HEIGHT], 8
    mov dx, 0x03C2                  ; Miscellaneous Output: colour, 25.175 MHz clock
    mov al, 0x63
    out dx, al
    mov dx, 0x03C4                  ; Sequencer memory mode: chain-4
    mov ax, 0x0E04
    call vid_out_idx
    mov dx, 0x03CE                  ; Graphics Controller: 256-colour shift mode,
    mov ax, 0x4005                  ; graphics at A0000
    call vid_out_idx
    mov ax, 0x0506
    call vid_out_idx
    mov si, vid_ac_13h              ; attribute controller and DAC
    call vid_load_ac
    call vid_load_dac_256
    test bh, 0x80
    jnz .sm_done
    mov ax, 0xA000                  ; clear the 64000-byte frame buffer
    mov es, ax
    xor di, di
    mov cx, 32000
    xor ax, ax
    rep stosw

.sm_done:
    pop ax
    pop bx
    pop cx
    pop dx
    pop di
    pop si
    pop es
    pop ds
    iret

; --- AH=01: Set cursor shape ---
.set_cursor_shape:
    push ds
    push ax
    push dx
    mov ax, 0x0040
    mov ds, ax
    mov [BDA_CURSOR_SHAPE], cx
    ; Program CRTC cursor start (R10) and end (R11)
    mov dx, [BDA_CRTC_BASE]   ; Read CRTC port from BDA
    mov al, 0x0A               ; Register 10: Cursor Start
    out dx, al
    inc dx
    mov al, ch                 ; Start scanline
    out dx, al
    dec dx
    mov al, 0x0B               ; Register 11: Cursor End
    out dx, al
    inc dx
    mov al, cl                 ; End scanline
    out dx, al
    pop dx
    pop ax
    pop ds
    iret

; --- AH=02: Set cursor position ---
; BH = page (0-7), DH/DL = row/column. The hardware cursor follows when BH is
; the active page.
.set_cursor_pos:
    cmp bh, 7
    ja .scp_ret
    push ds
    push ax
    push si
    mov ax, 0x0040
    mov ds, ax
    mov al, bh
    xor ah, ah
    shl ax, 1
    mov si, ax
    mov [BDA_CURSOR_POS + si], dx
    cmp bh, [BDA_ACTIVE_PAGE]
    jne .scp_done
    call vid_sync_cursor
.scp_done:
    pop si
    pop ax
    pop ds
.scp_ret:
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
; AL = page (0-7).
.set_active_page:
    cmp al, 7
    ja .sap_ret
    push ds
    push ax
    push bx
    push dx
    mov bx, 0x0040
    mov ds, bx
    mov [BDA_ACTIVE_PAGE], al
    xor ah, ah
    mul word [BDA_VIDEO_PGSZ]  ; AX = offset of the page in video memory
    mov [BDA_CRT_START], ax
    shr ax, 1                  ; the CRTC counts character cells
    mov bx, ax
    mov dx, [BDA_CRTC_BASE]
    mov al, 0x0C               ; Register 12: Start Address High
    mov ah, bh
    call vid_out_idx
    mov al, 0x0D               ; Register 13: Start Address Low
    mov ah, bl
    call vid_out_idx
    call vid_sync_cursor       ; show the cursor of the page now displayed
    pop dx
    pop bx
    pop ax
    pop ds
.sap_ret:
    iret

; --- AH=06: Scroll up ---
; AL = lines (0=clear window), BH = fill attribute
; CH/CL = top-left row/col, DH/DL = bottom-right row/col
; The window is clipped to the 25x80 screen; an empty one does nothing.
.scroll_up:
    push dx                 ; the caller's DX; the window is clipped in DX
    cmp dh, 24
    jbe .su_row_ok
    mov dh, 24
.su_row_ok:
    cmp dl, 79
    jbe .su_col_ok
    mov dl, 79
.su_col_ok:
    cmp ch, dh
    ja .su_ret
    cmp cl, dl
    ja .su_ret
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

    ; Calculate video segment with active page offset
    push ax
    mov ax, 0x0040
    mov ds, ax
    mov al, [BDA_ACTIVE_PAGE]
    xor ah, ah
    mov bx, [BDA_VIDEO_PGSZ]
    mul bx                      ; AX = page * page_size (bytes)
    mov cl, 4
    shr ax, cl                  ; AX = page offset in paragraphs
    add ax, 0xB800
    mov ds, ax
    mov es, ax
    pop ax

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
.su_ret:
    pop dx
    iret

; --- AH=07: Scroll down ---
; AL = lines (0=clear window), BH = fill attribute
; CH/CL = top-left row/col, DH/DL = bottom-right row/col
; The window is clipped to the 25x80 screen; an empty one does nothing.
.scroll_down:
    push dx                 ; the caller's DX; the window is clipped in DX
    cmp dh, 24
    jbe .sd_row_ok
    mov dh, 24
.sd_row_ok:
    cmp dl, 79
    jbe .sd_col_ok
    mov dl, 79
.sd_col_ok:
    cmp ch, dh
    ja .sd_ret
    cmp cl, dl
    ja .sd_ret
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

    ; Calculate video segment with active page offset
    push ax
    mov ax, 0x0040
    mov ds, ax
    mov al, [BDA_ACTIVE_PAGE]
    xor ah, ah
    mov bx, [BDA_VIDEO_PGSZ]
    mul bx                      ; AX = page * page_size (bytes)
    mov cl, 4
    shr ax, cl                  ; AX = page offset in paragraphs
    add ax, 0xB800
    mov ds, ax
    mov es, ax
    pop ax

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
.sd_ret:
    pop dx
    iret

; --- AH=08: Read char+attr at cursor ---
.read_char_attr:
    push ds
    push si
    push cx
    push dx
    mov cx, 0x0040
    mov ds, cx
    ; Save page number
    mov dl, bh
    ; Get cursor position for page BH
    mov al, bh
    xor ah, ah
    shl ax, 1
    mov si, ax
    mov al, byte [BDA_CURSOR_POS+1+si]  ; Row for page BH
    mov cl, 80
    mul cl
    xor ch, ch
    mov cl, byte [BDA_CURSOR_POS+si]    ; Col for page BH
    add ax, cx
    mov si, ax
    add si, si
    ; Calculate page offset: page * BDA_VIDEO_PGSZ
    mov al, dl                  ; Page number saved earlier
    xor ah, ah
    mov cx, [BDA_VIDEO_PGSZ]
    mul cx                      ; AX = page * page_size
    add si, ax                  ; Add page offset to cursor offset
    mov cx, 0xB800
    mov ds, cx
    lodsw                       ; AL = char, AH = attr
    pop dx
    pop cx
    pop si
    pop ds
    iret

; --- AH=09: Write char+attr at cursor ---
; AL = character, BH = page, BL = attribute, CX = count (0 writes nothing)
.write_char_attr:
    jcxz .wa_ret
    push es
    push di
    push dx
    push ds
    push cx
    push ax
    push bx
    mov dx, 0x0040
    mov ds, dx
    ; Get cursor position for page BH
    mov al, bh
    xor ah, ah
    shl ax, 1
    mov di, ax
    mov dh, byte [BDA_CURSOR_POS+1+di]
    mov dl, byte [BDA_CURSOR_POS+di]
    ; Save cursor pos, compute page offset
    push dx                     ; Save cursor (DH=row, DL=col)
    mov al, bh
    xor ah, ah
    mov di, [BDA_VIDEO_PGSZ]
    mul di                      ; AX = page * page_size (clobbers DX)
    mov di, ax                  ; DI = page offset
    pop dx                      ; Restore cursor (DH=row, DL=col)
    ; Calculate cursor offset within page
    mov al, dh                  ; AL = row
    xor ah, ah
    push dx
    push cx                     ; Save count for loop
    mov cx, 80
    mul cx                      ; AX = row * 80
    pop cx                      ; Restore count
    pop dx
    xor dh, dh                  ; DX = col only
    add ax, dx
    add ax, ax                  ; Convert to byte offset
    add di, ax                  ; DI = page offset + cursor offset
    pop bx
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
.wa_ret:
    iret

; --- AH=0A: Write char at cursor (keep attr) ---
; AL = character, BH = page, CX = count (0 writes nothing)
.write_char:
    jcxz .wc_ret
    push es
    push di
    push dx
    push ds
    push cx
    push ax
    push bx
    mov dx, 0x0040
    mov ds, dx
    ; Get cursor position for page BH
    mov al, bh
    xor ah, ah
    shl ax, 1
    mov di, ax
    mov dh, byte [BDA_CURSOR_POS+1+di]
    mov dl, byte [BDA_CURSOR_POS+di]
    ; Save cursor pos, compute page offset
    push dx                     ; Save cursor (DH=row, DL=col)
    mov al, bh
    xor ah, ah
    mov di, [BDA_VIDEO_PGSZ]
    mul di                      ; AX = page * page_size (clobbers DX)
    mov di, ax                  ; DI = page offset
    pop dx                      ; Restore cursor (DH=row, DL=col)
    ; Calculate cursor offset within page
    mov al, dh                  ; AL = row
    xor ah, ah
    push dx
    push cx                     ; Save count for loop
    mov cx, 80
    mul cx                      ; AX = row * 80
    pop cx                      ; Restore count
    pop dx
    xor dh, dh                  ; DX = col only
    add ax, dx
    add ax, ax                  ; Convert to byte offset
    add di, ax                  ; DI = page offset + cursor offset
    pop bx
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
.wc_ret:
    iret

; --- AH=0B: Set color palette (text mode no-op) ---
.set_palette:
    iret

; --- AH=0E: Teletype output ---
; Writes AL at the cursor of the active page and advances the cursor; CR, LF,
; BS and BEL are acted on instead of written. The attribute already on screen
; is kept. When the cursor moves past the last row the page scrolls up one
; line, and the new line takes the attribute found at the cursor.
.teletype:
    push ds
    push es
    push bx
    push cx
    push dx
    push di
    push si

    out 0xE9, al

    mov bx, 0x0040
    mov ds, bx
    mov bl, [BDA_ACTIVE_PAGE]
    and bx, 0x0007
    mov si, bx
    shl si, 1                       ; SI = the page's cursor slot
    mov cl, 8
    shl bx, cl                      ; pages are 4096 bytes = 100h paragraphs apart
    add bx, 0xB800
    mov es, bx                      ; ES:0000 = first cell of the active page
    mov dx, [BDA_CURSOR_POS + si]   ; DH = row, DL = column

    cmp al, 0x0D
    je .t_cr
    cmp al, 0x0A
    je .t_lf
    cmp al, 0x08
    je .t_bs
    cmp al, 0x07
    je .t_done

    call .t_cell
    mov [es:di], al
    inc dl
    cmp dl, 80
    jb .t_update
    xor dl, dl
    jmp .t_lf

.t_cr:
    xor dl, dl
    jmp .t_update

.t_bs:
    cmp dl, 0
    je .t_update
    dec dl
    jmp .t_update

.t_lf:
    inc dh
    cmp dh, 25
    jb .t_update
    mov dh, 24
    ; Scroll the page up one line
    push ax
    push si
    push ds
    call .t_cell
    mov ah, [es:di+1]               ; attribute at the cursor, for the new line
    mov al, ' '
    push es
    pop ds
    mov si, 160
    xor di, di
    mov cx, 1920
    cld
    rep movsw
    mov cx, 80
    rep stosw
    pop ds
    pop si
    pop ax

.t_update:
    mov [BDA_CURSOR_POS + si], dx
    call vid_sync_cursor

.t_done:
    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop es
    pop ds
    iret

; DI = offset within the page of the cell at row DH, column DL.
.t_cell:
    push ax
    mov al, 80
    mul dh
    mov di, dx
    and di, 0x00FF
    add di, ax
    shl di, 1
    pop ax
    ret

; --- AH=0F: Get video mode ---
; Returns AL = mode, AH = columns, BH = active page.
.get_mode:
    push ds
    mov ax, 0x0040
    mov ds, ax
    mov al, [BDA_VIDEO_MODE]
    mov ah, byte [BDA_VIDEO_COLS]
    mov bh, [BDA_ACTIVE_PAGE]
    pop ds
    iret

; --- AH=13: Write string ---
; AL = write mode (bit 0: leave the cursor after the string, bit 1: the
;      string holds character/attribute pairs)
; BH = page, BL = attribute (when AL bit 1 = 0)
; CX = number of characters
; DH = row, DL = column of the first character
; ES:BP = pointer to the string
;
; CR, LF, BS and BEL move the cursor instead of being written, and carry no
; attribute byte. Without AL bit 0 the cursor ends up where it was before
; the call. A line feed on the last row scrolls through the teletype
; function, i.e. it scrolls the active page.
.write_string:
    push ds
    push si
    push di
    push dx
    push cx
    push bx
    push ax
    cld
    cmp bh, 7
    ja .ws_exit
    jcxz .ws_exit
    mov di, cx                      ; DI = characters left
    mov ch, al                      ; CH = write mode
    mov si, 0x0040
    mov ds, si
    mov al, bh
    xor ah, ah
    shl ax, 1
    mov si, ax
    push word [BDA_CURSOR_POS + si] ; the page's cursor before the call
    push es
    pop ds
    mov si, bp                      ; DS:SI -> string
    mov ah, 0x02
    int 0x10                        ; cursor to the start position
.ws_loop:
    lodsb
    cmp al, 0x0D
    je .ws_cr
    cmp al, 0x0A
    je .ws_lf
    cmp al, 0x08
    je .ws_bs
    cmp al, 0x07
    je .ws_next
    test ch, 0x02
    jz .ws_put
    mov bl, [si]                    ; the attribute follows the character
    inc si
.ws_put:
    push cx
    mov cx, 1
    mov ah, 0x09
    int 0x10                        ; character and attribute at the cursor
    pop cx
    inc dl
    cmp dl, 80
    jb .ws_set_cursor
    xor dl, dl
.ws_lf:
    inc dh
    cmp dh, 25
    jb .ws_set_cursor
    mov dh, 24
    mov ah, 0x02
    int 0x10                        ; cursor on the last row,
    mov ax, 0x0E0A
    int 0x10                        ; then a teletype line feed scrolls
    jmp .ws_next
.ws_cr:
    xor dl, dl
    jmp .ws_set_cursor
.ws_bs:
    or dl, dl
    jz .ws_set_cursor
    dec dl
.ws_set_cursor:
    mov ah, 0x02
    int 0x10
.ws_next:
    dec di
    jnz .ws_loop
    pop dx                          ; the cursor before the call
    test ch, 0x01
    jnz .ws_exit
    mov ah, 0x02
    int 0x10
.ws_exit:
    pop ax
    pop bx
    pop cx
    pop dx
    pop di
    pop si
    pop ds
    iret

; --- AH=10: Palette functions ---
; AL = 03h  blinking (BL = 1) or bright backgrounds (BL = 0) for attribute bit 7
; AL = 10h  set DAC register BX to DH, CH, CL (red, green, blue)
; AL = 12h  set CX DAC registers starting at BX from the table at ES:DX
; AL = 15h  read DAC register BL into DH, CH, CL
; AL = 17h  read CX DAC registers starting at BX into the buffer at ES:DX
; DAC components are 6 bits each. Other subfunctions do nothing.
.palette:
    cmp al, 0x03
    je .pal_blink
    cmp al, 0x10
    je .pal_set_register
    cmp al, 0x12
    je .pal_set_block
    cmp al, 0x15
    je .pal_get_register
    cmp al, 0x17
    je .pal_get_block
    iret

.pal_blink:
    push ds
    push dx
    push ax
    mov ax, 0x0040
    mov ds, ax
    ; Attribute controller mode control register, bit 3
    mov dx, 0x03DA
    in al, dx                       ; the next write to 3C0h selects a register
    mov dx, 0x03C0
    mov al, 0x10
    out dx, al
    inc dx
    in al, dx                       ; read the register back through 3C1h
    dec dx
    and al, 0xF7
    test bl, 0x01
    jz .pal_blink_ac
    or al, 0x08
.pal_blink_ac:
    out dx, al
    mov al, 0x20                    ; index with bit 5 set: display on
    out dx, al
    mov dx, 0x03DA
    in al, dx                       ; leave 3C0h expecting a register number
    ; CGA mode control register, bit 5, and its copy in the BDA
    mov al, [BDA_CRT_MODE_SET]
    and al, 0xDF
    test bl, 0x01
    jz .pal_blink_cga
    or al, 0x20
.pal_blink_cga:
    mov [BDA_CRT_MODE_SET], al
    mov dx, 0x03D8
    out dx, al
    pop ax
    pop dx
    pop ds
    iret

.pal_set_register:
    push dx
    push ax
    mov ah, dh                      ; red
    mov dx, 0x03C8
    mov al, bl
    out dx, al
    inc dx
    mov al, ah
    out dx, al
    mov al, ch                      ; green
    out dx, al
    mov al, cl                      ; blue
    out dx, al
    pop ax
    pop dx
    iret

.pal_set_block:
    push si
    push dx
    push cx
    push ax
    cld
    mov si, dx
    mov dx, 0x03C8
    mov al, bl
    out dx, al
    inc dx
    mov ax, cx
    shl cx, 1
    add cx, ax                      ; three bytes per register
    jcxz .pal_set_block_done
.pal_set_block_next:
    es lodsb
    out dx, al
    loop .pal_set_block_next
.pal_set_block_done:
    pop ax
    pop cx
    pop dx
    pop si
    iret

.pal_get_register:
    push ax
    push bx
    mov bh, dl                      ; DL is not a result: keep it
    mov dx, 0x03C7
    mov al, bl
    out dx, al
    mov dx, 0x03C9
    in al, dx
    mov ah, al                      ; red
    in al, dx
    mov ch, al                      ; green
    in al, dx
    mov cl, al                      ; blue
    mov dh, ah
    mov dl, bh
    pop bx
    pop ax
    iret

.pal_get_block:
    push di
    push dx
    push cx
    push ax
    cld
    mov di, dx
    mov dx, 0x03C7
    mov al, bl
    out dx, al
    mov dx, 0x03C9
    mov ax, cx
    shl cx, 1
    add cx, ax                      ; three bytes per register
    jcxz .pal_get_block_done
.pal_get_block_next:
    in al, dx
    stosb
    loop .pal_get_block_next
.pal_get_block_done:
    pop ax
    pop cx
    pop dx
    pop di
    iret

; --- AH=0C: Write pixel / AH=0D: Read pixel ---
; AL = colour (written, or returned), CX = column, DX = row.
; Only mode 13h has pixels: in any other mode, or outside the 320x200 screen,
; a write is ignored and a read returns 0.
.write_pixel:
    push ds
    push bx
    push dx
    push ax
    mov bx, 0x0040
    mov ds, bx
    cmp byte [BDA_VIDEO_MODE], 0x13
    jne .wp_done
    cmp cx, 320
    jae .wp_done
    cmp dx, 200
    jae .wp_done
    mov ax, 320
    mul dx                          ; AX = row * 320
    add ax, cx
    mov bx, ax
    mov ax, 0xA000
    mov ds, ax
    pop ax
    push ax
    mov [bx], al
.wp_done:
    pop ax
    pop dx
    pop bx
    pop ds
    iret

.read_pixel:
    push ds
    push bx
    push dx
    mov bx, 0x0040
    mov ds, bx
    xor al, al
    cmp byte [BDA_VIDEO_MODE], 0x13
    jne .rp_done
    cmp cx, 320
    jae .rp_done
    cmp dx, 200
    jae .rp_done
    push ax
    mov ax, 320
    mul dx                          ; AX = row * 320
    add ax, cx
    mov bx, ax
    mov ax, 0xA000
    mov ds, ax
    pop ax
    mov al, [bx]
.rp_done:
    pop dx
    pop bx
    pop ds
    iret

; =============================================================================
; Video helpers (near routines used by the INT 10h handler)
; =============================================================================

; Writes AH to register AL of the index/data port pair at DX/DX+1.
; DX is preserved; AL is destroyed.
vid_out_idx:
    out dx, al
    inc dx
    mov al, ah
    out dx, al
    dec dx
    ret

; Programs the CRTC cursor location with the cursor of the active page: the
; page's first cell plus row * 80 + column. Needs DS = 0040h.
; Preserves all registers.
vid_sync_cursor:
    push ax
    push bx
    push dx
    mov bl, [BDA_ACTIVE_PAGE]
    xor bh, bh
    shl bx, 1
    mov bx, [BDA_CURSOR_POS + bx]   ; BH = row, BL = column
    mov al, 80
    mul bh                          ; AX = row * 80
    xor bh, bh
    add bx, ax
    mov ax, [BDA_CRT_START]
    shr ax, 1
    add bx, ax                      ; + first cell of the active page
    mov dx, [BDA_CRTC_BASE]
    mov al, 0x0E
    mov ah, bh
    call vid_out_idx
    mov al, 0x0F
    mov ah, bl
    call vid_out_idx
    pop dx
    pop bx
    pop ax
    ret

; CRTC registers every mode set programs: register number, value.
vid_crtc_reset:
    db 0x0A, 0x06,  0x0B, 0x07      ; cursor on scan lines 6-7
    db 0x0C, 0x00,  0x0D, 0x00      ; display starts at the first cell
    db 0x0E, 0x00,  0x0F, 0x00      ; cursor at the first cell

; Loads attribute controller registers 00h-14h (the 16 palette registers, mode
; control, overscan, plane enable, pixel panning, colour select) from the
; table at CS:SI, turns the display back on and opens the DAC pixel mask.
; Needs DF = 0. Destroys AX, CX, DX, SI.
vid_load_ac:
    mov dx, 0x03DA
    in al, dx                       ; the next write to 3C0h selects a register
    mov dx, 0x03C0
    xor ah, ah                      ; AH = register number
    mov cx, 21
.next:
    mov al, ah
    out dx, al
    cs lodsb
    out dx, al
    inc ah
    loop .next
    mov al, 0x20                    ; index with bit 5 set: display on
    out dx, al
    mov dx, 0x03DA
    in al, dx                       ; leave 3C0h expecting a register number
    mov dx, 0x03C6
    mov al, 0xFF                    ; DAC pixel mask: all bits
    out dx, al
    ret

; Text modes: DAC registers 00h-3Fh get the 64 EGA colours - bits 2, 1, 0 of
; the register number are red, green, blue at 2/3 intensity and bits 5, 4, 3
; the same at 1/3 - and the rest of the DAC is black.
; Destroys AX, CX, DX.
vid_load_dac_ega:
    mov dx, 0x03C8
    xor al, al
    out dx, al                      ; start at DAC register 0
    inc dx
    xor ah, ah                      ; AH = register number
.colour:
    mov cx, 0x2004                  ; red:   CL = 2/3 bit, CH = 1/3 bit
    call .component
    mov cx, 0x1002                  ; green
    call .component
    mov cx, 0x0801                  ; blue
    call .component
    inc ah
    cmp ah, 64
    jb .colour
    xor al, al
    mov cx, (256 - 64) * 3
.black:
    out dx, al
    loop .black
    ret
.component:
    xor al, al
    test ah, cl
    jz .no_two_thirds
    add al, 0x2A
.no_two_thirds:
    test ah, ch
    jz .no_third
    add al, 0x15
.no_third:
    out dx, al
    ret

; Mode 13h: the default 256-colour palette. Needs DF = 0.
; Destroys AX, CX, DX, SI.
vid_load_dac_256:
    mov dx, 0x03C8
    xor al, al
    out dx, al                      ; start at DAC register 0
    inc dx
    mov si, vga_palette_256
    mov cx, 256 * 3
.next:
    cs lodsb
    out dx, al
    loop .next
    ret

; Attribute controller registers 00h-14h of the text modes. The palette maps
; the 16 text colours to EGA colours: 00h-05h and 07h directly, brown to 14h,
; the bright colours to 38h-3Fh. Then: mode control (text, line graphics,
; blink), overscan, plane enable, pixel panning, colour select.
vid_ac_text:
    db 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07
    db 0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F
    db 0x0C, 0x00, 0x0F, 0x08, 0x00

; The same registers for mode 13h: identity palette, 256-colour graphics.
vid_ac_13h:
    db 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07
    db 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F
    db 0x41, 0x00, 0x0F, 0x00, 0x00

%include "vga_palette.inc"
