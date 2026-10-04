; =============================================================================
; INT 09h — Keyboard (IRQ1)
;
; Turns PC/XT scancodes into keystrokes (scan code in AH, character in AL)
; and queues them in the BIOS keyboard buffer. Shift, Ctrl, Alt and the lock
; keys are tracked in the BDA keyboard flags.
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

    ; --- Ordinary key: AH = scan code, BL = shift flags ---
    mov ah, al
    mov bl, [BDA_KBD_FLAGS1]
    test bl, 0x08
    jnz .k_alt
    test bl, 0x04
    jnz .k_ctrl
    cmp al, 0x47
    jae .k_keypad
    cmp al, 0x3B
    jae .k_fkey

    ; --- Typewriter keys 01h-39h ---
    mov si, scancode_table
    test bl, 0x03
    jz .k_lookup
    cmp al, 0x0F             ; Shift+Tab is back-tab: scan 0Fh, no character
    je .k_no_char
    mov si, scancode_shift_table
.k_lookup:
    xor bh, bh
    mov bl, al
    cmp bl, scancode_table_end - scancode_table
    jae .k_done
    cs mov al, [si+bx]
    cmp al, 0
    je .k_done               ; the key has no character
    ; Caps Lock swaps the case of letters only
    test byte [BDA_KBD_FLAGS1], 0x40
    jz .k_insert
    mov bl, al
    or bl, 0x20
    cmp bl, 'a'
    jb .k_insert
    cmp bl, 'z'
    ja .k_insert
    xor al, 0x20
    jmp .k_insert

.k_no_char:
    xor al, al
    jmp .k_insert

    ; --- F1-F10 ---
.k_fkey:
    cmp al, 0x44
    ja .k_done               ; 45h/46h are lock keys, handled above
    test bl, 0x03
    jz .k_no_char
    add ah, 0x19             ; Shift+F1-F10: 54h-5Dh
    jmp .k_no_char

    ; --- Cursor and keypad keys 47h-53h ---
.k_keypad:
    cmp al, 0x53
    ja .k_done
    cmp al, 0x4A
    jne .k_keypad_plus
    mov al, '-'
    jmp .k_insert
.k_keypad_plus:
    cmp al, 0x4E
    jne .k_no_char
    mov al, '+'
    jmp .k_insert

    ; --- Alt + key ---
.k_alt:
    test bl, 0x04
    jz .k_alt_space
    cmp al, 0x53             ; Ctrl+Alt+Del restarts the machine
    jne .k_alt_space
    mov word [BDA_RESET_FLAG], 0x1234
    jmp 0xFFFF:0x0000
.k_alt_space:
    cmp al, 0x39             ; Alt+Space is a space
    jne .k_alt_tab
    mov al, ' '
    jmp .k_insert
.k_alt_tab:
    cmp al, 0x0F             ; Alt+Tab: A5h (0Fh without a character is back-tab)
    jne .k_alt_digits
    mov ah, 0xA5
    jmp .k_no_char
.k_alt_digits:
    cmp al, 0x02
    jb .k_no_char            ; Alt+Esc
    cmp al, 0x0D
    ja .k_alt_fkeys
    add ah, 0x76             ; Alt+1 ... Alt+=: 78h-83h
    jmp .k_no_char
.k_alt_fkeys:
    cmp al, 0x3B
    jb .k_no_char            ; letters and punctuation keep their scan code
    cmp al, 0x44
    ja .k_alt_keypad
    add ah, 0x2D             ; Alt+F1-F10: 68h-71h
    jmp .k_no_char
.k_alt_keypad:
    cmp al, 0x47
    jb .k_done
    cmp al, 0x53
    ja .k_done
    cmp al, 0x4A             ; Alt with keypad -, 5 and + produces nothing
    je .k_done
    cmp al, 0x4C
    je .k_done
    cmp al, 0x4E
    je .k_done
    add ah, 0x50             ; Alt + cursor keys: 97h-A3h
    jmp .k_no_char

    ; --- Ctrl + key ---
.k_ctrl:
    cmp al, 0x47
    jae .k_ctrl_keypad
    cmp al, 0x3B
    jae .k_ctrl_fkey
    cmp al, 0x37             ; Ctrl+PrtSc: 7200h
    jne .k_ctrl_lookup
    mov ah, 0x72
    jmp .k_no_char
.k_ctrl_lookup:
    xor bh, bh
    mov bl, al
    cmp bl, ctrl_table_end - ctrl_table
    jae .k_done
    cs mov al, [ctrl_table + bx]
    cmp al, 0xFF
    je .k_done               ; the combination produces nothing
    jmp .k_insert
.k_ctrl_fkey:
    cmp al, 0x44
    ja .k_done
    add ah, 0x23             ; Ctrl+F1-F10: 5Eh-67h
    jmp .k_no_char
.k_ctrl_keypad:
    cmp al, 0x53
    ja .k_done
    xor bh, bh
    mov bl, al
    cs mov ah, [ctrl_keypad_table + bx - 0x47]
    jmp .k_no_char

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
    test byte [BDA_KBD_FLAGS1], 0x04
    jz .k_scroll_toggle
    ; Ctrl+Scroll Lock is Break: empty the buffer, raise the break flag, call
    ; the break handler and queue a 0000h keystroke
    mov bx, [BDA_KBD_BUF_START]
    mov [BDA_KBD_BUF_HEAD], bx
    mov [BDA_KBD_BUF_TAIL], bx
    mov byte [BDA_BREAK_FLAG], 0x80
    int 0x1B
    xor ax, ax
    jmp .k_insert
.k_scroll_toggle:
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
    mov ax, 0x5200                     ; Insert is a keystroke too: scan 52h, no character
    jmp .k_insert

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
    db 0x08, 0                                       ; 0x0E-0x0F (BS; Shift+Tab is handled in code)
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

; --- Characters queued for Ctrl + the typewriter keys 00h-39h ---
; The control characters of the IBM PC/XT BIOS; 0xFF = the combination
; produces nothing.
ctrl_table:
    db 0xFF                                           ; 0x00
    db 0x1B, 0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0x1E, 0xFF ; 0x01-0x08 (Esc; 2 = NUL; 6 = 1Eh)
    db 0xFF, 0xFF, 0xFF, 0x1F, 0xFF, 0x7F, 0xFF       ; 0x09-0x0F (- = 1Fh; BS = 7Fh)
    db 0x11, 0x17, 0x05, 0x12, 0x14, 0x19, 0x15, 0x09 ; 0x10-0x17 (Q W E R T Y U I)
    db 0x0F, 0x10, 0x1B, 0x1D, 0x0A, 0xFF             ; 0x18-0x1D (O P; [ = 1Bh; ] = 1Dh; Enter = LF)
    db 0x01, 0x13, 0x04, 0x06, 0x07, 0x08, 0x0A, 0x0B ; 0x1E-0x25 (A S D F G H J K)
    db 0x0C, 0xFF, 0xFF, 0xFF, 0xFF, 0x1C             ; 0x26-0x2B (L; \ = 1Ch)
    db 0x1A, 0x18, 0x03, 0x16, 0x02, 0x0E, 0x0D       ; 0x2C-0x32 (Z X C V B N M)
    db 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, ' '        ; 0x33-0x39 (Space)
ctrl_table_end:

; --- Scan codes queued for Ctrl + the cursor/keypad keys 47h-53h ---
; 73h-77h and 84h are the PC/XT codes; the others are the codes the enhanced
; keyboard BIOS uses.
ctrl_keypad_table:
    db 0x77, 0x8D, 0x84, 0x8E, 0x73, 0x8F, 0x74      ; Home Up PgUp - Left 5 Right
    db 0x90, 0x75, 0x91, 0x76, 0x92, 0x93            ; + End Down PgDn Ins Del
