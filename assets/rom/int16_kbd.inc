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
    cmp ah, 0x10
    je .kb_read
    cmp ah, 0x11
    je .kb_status
    cmp ah, 0x12
    je .kb_eflags
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
    push bx
    mov bx, 0x0040
    mov ds, bx
    mov al, [BDA_KBD_FLAGS1]
    pop bx
    pop ds
    iret

; --- AH=12h: Get enhanced shift flags ---
; Returns AL = BDA_KBD_FLAGS1, AH = BDA_KBD_FLAGS2
.kb_eflags:
    push ds
    push bx
    mov bx, 0x0040
    mov ds, bx
    mov al, [BDA_KBD_FLAGS1]
    mov ah, [BDA_KBD_FLAGS2]
    pop bx
    pop ds
    iret
