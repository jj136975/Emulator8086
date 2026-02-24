; =============================================================================
; INT 09h — Keyboard (IRQ1)
; =============================================================================
int09_handler:
    push ax
    push bx
    push cx
    push si
    push ds

    mov ax, 0x0040
    mov ds, ax

    in al, 0x60

    ; ACK keyboard
    xchg al, ah
    in al, 0x61
    or al, 0x80
    out 0x61, al
    and al, 0x7F
    out 0x61, al
    xchg al, ah             ; AL = scancode

    ; INT 15h AH=4Fh keyboard intercept
    mov ah, 0x4F
    stc
    int 0x15
    jnc .k_done              ; CF=0 means scancode consumed

    ; AL = scancode (possibly modified by INT 15h handler)

    ; --- Break codes (key release) ---
    test al, 0x80
    jnz .k_break

    ; --- Modifier make codes ---
    cmp al, 0x2A
    je .k_lshift_on
    cmp al, 0x36
    je .k_rshift_on
    cmp al, 0x1D
    je .k_ctrl_on
    cmp al, 0x38
    je .k_alt_on
    cmp al, 0x3A
    je .k_capslock
    cmp al, 0x45
    je .k_numlock
    cmp al, 0x46
    je .k_scrolllock
    cmp al, 0x52
    je .k_ins_toggle

    ; --- Regular key ---
    mov cl, al               ; CL = scancode

    ; Special keys (F1-F10, nav) handled separately
    cmp cl, 0x3B
    jae .k_special

    ; Check Alt
    test byte [BDA_KBD_FLAGS1], 0x08
    jnz .k_alt_regular

    ; Check Ctrl
    test byte [BDA_KBD_FLAGS1], 0x04
    jnz .k_ctrl_regular

    ; --- Normal/shifted lookup ---
    xor ch, ch               ; CH = effective shift flag
    test byte [BDA_KBD_FLAGS1], 0x03
    jz .k_no_shift
    inc ch
.k_no_shift:
    ; Caps Lock toggles shift for letter keys only
    test byte [BDA_KBD_FLAGS1], 0x40
    jz .k_do_lookup
    cmp cl, 0x10
    jb .k_do_lookup
    cmp cl, 0x19
    jbe .k_is_letter
    cmp cl, 0x1E
    jb .k_do_lookup
    cmp cl, 0x26
    jbe .k_is_letter
    cmp cl, 0x2C
    jb .k_do_lookup
    cmp cl, 0x32
    ja .k_do_lookup
.k_is_letter:
    xor ch, 1

.k_do_lookup:
    mov ah, cl               ; AH = scancode for buffer
    cmp ch, 0
    jne .k_shifted

    ; Unshifted lookup
    xor bx, bx
    mov bl, cl
    cmp bl, scancode_table_end - scancode_table
    jae .k_done
    mov si, scancode_table
    cs mov al, [si+bx]
    cmp al, 0
    je .k_done
    jmp .k_insert

.k_shifted:
    xor bx, bx
    mov bl, cl
    cmp bl, scancode_shift_end - scancode_shift_table
    jae .k_done
    mov si, scancode_shift_table
    cs mov al, [si+bx]
    cmp al, 0
    je .k_done
    jmp .k_insert

    ; --- Alt+regular key ---
.k_alt_regular:
    mov ah, cl
    xor al, al
    jmp .k_insert

    ; --- Ctrl+regular key ---
.k_ctrl_regular:
    mov ah, cl
    xor bx, bx
    mov bl, cl
    cmp bl, scancode_table_end - scancode_table
    jae .k_done
    mov si, scancode_table
    cs mov al, [si+bx]
    cmp al, 'a'
    jb .k_insert             ; non-letter: insert as-is
    cmp al, 'z'
    ja .k_insert
    sub al, 0x60             ; Ctrl+A=0x01, Ctrl+B=0x02, ...
    jmp .k_insert

    ; --- Special keys (F1-F10, nav) ---
.k_special:
    cmp cl, 0x44
    jbe .k_fkey
    cmp cl, 0x47
    jb .k_done               ; 0x45/0x46 already handled as modifiers
    cmp cl, 0x53
    ja .k_done

    ; Nav keys (Home, Up, PgUp, Left, Right, End, Down, PgDn, Ins, Del)
    mov ah, cl
    xor al, al
    jmp .k_insert

.k_fkey:
    ; F1-F10: adjust scancode based on modifiers
    mov ah, cl
    xor al, al
    test byte [BDA_KBD_FLAGS1], 0x08
    jnz .k_fkey_alt
    test byte [BDA_KBD_FLAGS1], 0x04
    jnz .k_fkey_ctrl
    test byte [BDA_KBD_FLAGS1], 0x03
    jnz .k_fkey_shift
    jmp .k_insert            ; unmodified F-key

.k_fkey_shift:
    add ah, 0x19             ; F1: 0x3B -> 0x54
    jmp .k_insert
.k_fkey_ctrl:
    add ah, 0x23             ; F1: 0x3B -> 0x5E
    jmp .k_insert
.k_fkey_alt:
    add ah, 0x2D             ; F1: 0x3B -> 0x68
    jmp .k_insert

    ; --- Buffer insertion ---
.k_insert:
    ; AH = scancode, AL = ASCII
    mov bx, [BDA_KBD_BUF_TAIL]
    mov si, bx
    add si, 2
    cmp si, [BDA_KBD_BUF_END]
    jb .k_nw
    mov si, [BDA_KBD_BUF_START]
.k_nw:
    cmp si, [BDA_KBD_BUF_HEAD]
    je .k_done               ; buffer full
    mov [bx], ax
    mov [BDA_KBD_BUF_TAIL], si
    jmp .k_done

    ; --- Modifier set ---
.k_lshift_on:
    or byte [BDA_KBD_FLAGS1], 0x02
    jmp .k_done
.k_rshift_on:
    or byte [BDA_KBD_FLAGS1], 0x01
    jmp .k_done
.k_ctrl_on:
    or byte [BDA_KBD_FLAGS1], 0x04
    jmp .k_done
.k_alt_on:
    or byte [BDA_KBD_FLAGS1], 0x08
    jmp .k_done
.k_capslock:
    test byte [BDA_KBD_FLAGS2], 0x40  ; Already held?
    jnz .k_done                        ; Yes, skip toggle
    or byte [BDA_KBD_FLAGS2], 0x40    ; Set held bit
    xor byte [BDA_KBD_FLAGS1], 0x40   ; Toggle Caps Lock
    jmp .k_done
.k_numlock:
    test byte [BDA_KBD_FLAGS2], 0x20  ; Already held?
    jnz .k_done                        ; Yes, skip toggle
    or byte [BDA_KBD_FLAGS2], 0x20    ; Set held bit
    xor byte [BDA_KBD_FLAGS1], 0x20   ; Toggle Num Lock
    jmp .k_done
.k_scrolllock:
    test byte [BDA_KBD_FLAGS2], 0x10  ; Already held?
    jnz .k_done                        ; Yes, skip toggle
    or byte [BDA_KBD_FLAGS2], 0x10    ; Set held bit
    xor byte [BDA_KBD_FLAGS1], 0x10   ; Toggle Scroll Lock
    jmp .k_done
.k_ins_toggle:
    test byte [BDA_KBD_FLAGS2], 0x80  ; Already held?
    jnz .k_done                        ; Yes, skip toggle
    or byte [BDA_KBD_FLAGS2], 0x80    ; Set held bit
    xor byte [BDA_KBD_FLAGS1], 0x80   ; Toggle Insert
    jmp .k_done

    ; --- Break code handling ---
.k_break:
    and al, 0x7F
    cmp al, 0x2A
    je .k_lshift_off
    cmp al, 0x36
    je .k_rshift_off
    cmp al, 0x1D
    je .k_ctrl_off
    cmp al, 0x38
    je .k_alt_off
    cmp al, 0x3A
    je .k_capslock_off
    cmp al, 0x45
    je .k_numlock_off
    cmp al, 0x46
    je .k_scrolllock_off
    cmp al, 0x52
    je .k_ins_toggle_off
    jmp .k_done

.k_lshift_off:
    and byte [BDA_KBD_FLAGS1], 0xFD
    jmp .k_done
.k_rshift_off:
    and byte [BDA_KBD_FLAGS1], 0xFE
    jmp .k_done
.k_ctrl_off:
    and byte [BDA_KBD_FLAGS1], 0xFB
    jmp .k_done
.k_alt_off:
    and byte [BDA_KBD_FLAGS1], 0xF7
    jmp .k_done
.k_capslock_off:
    and byte [BDA_KBD_FLAGS2], ~0x40  ; Clear held bit
    jmp .k_done
.k_numlock_off:
    and byte [BDA_KBD_FLAGS2], ~0x20  ; Clear held bit
    jmp .k_done
.k_scrolllock_off:
    and byte [BDA_KBD_FLAGS2], ~0x10  ; Clear held bit
    jmp .k_done
.k_ins_toggle_off:
    and byte [BDA_KBD_FLAGS2], ~0x80  ; Clear held bit
    jmp .k_done

.k_done:
    mov al, 0x20
    out 0x20, al
    pop ds
    pop si
    pop cx
    pop bx
    pop ax
    iret

; --- Unshifted scancode-to-ASCII table ---
scancode_table:
    db 0, 0x1B                                      ; 0x00-0x01 (null, Esc)
    db '1','2','3','4','5','6','7','8','9','0'       ; 0x02-0x0B
    db '-','='                                       ; 0x0C-0x0D
    db 0x08, 0x09                                    ; 0x0E-0x0F (BS, Tab)
    db 'q','w','e','r','t','y','u','i','o','p'       ; 0x10-0x19
    db '[',']'                                       ; 0x1A-0x1B
    db 0x0D, 0                                       ; 0x1C-0x1D (Enter, Ctrl)
    db 'a','s','d','f','g','h','j','k','l'           ; 0x1E-0x26
    db ';', 0x27, '`'                                ; 0x27-0x29
    db 0, 0x5C                                       ; 0x2A-0x2B (LShift, \)
    db 'z','x','c','v','b','n','m'                   ; 0x2C-0x32
    db ',','.','/'                                   ; 0x33-0x35
    db 0, '*', 0, ' '                                ; 0x36-0x39 (RShift, *, Alt, Space)
scancode_table_end:

; --- Shifted scancode-to-ASCII table ---
scancode_shift_table:
    db 0, 0x1B                                      ; 0x00-0x01
    db '!','@','#','$','%','^','&','*','(',')'       ; 0x02-0x0B
    db '_','+'                                       ; 0x0C-0x0D
    db 0x08, 0                                       ; 0x0E-0x0F (BS, Shift+Tab)
    db 'Q','W','E','R','T','Y','U','I','O','P'       ; 0x10-0x19
    db '{','}'                                       ; 0x1A-0x1B
    db 0x0D, 0                                       ; 0x1C-0x1D (Enter, Ctrl)
    db 'A','S','D','F','G','H','J','K','L'           ; 0x1E-0x26
    db ':', 0x22, '~'                                ; 0x27-0x29 (: " ~)
    db 0, '|'                                        ; 0x2A-0x2B (LShift, |)
    db 'Z','X','C','V','B','N','M'                   ; 0x2C-0x32
    db '<','>','?'                                   ; 0x33-0x35
    db 0, '*', 0, ' '                                ; 0x36-0x39
scancode_shift_end:
