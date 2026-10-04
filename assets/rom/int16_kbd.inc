; =============================================================================
; INT 16h — Keyboard Services
;
; Interrupts are enabled on entry, whatever the caller's interrupt flag, so
; that a pending keyboard interrupt gets serviced: a caller polling for a key
; with interrupts off would otherwise never see one.
; =============================================================================
int16_handler:
    sti
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

; --- AH=00h/10h: Wait for a keystroke and return it in AX ---
.kb_read:
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
    pop bx
    pop ds
    iret                     ; restores the caller's interrupt flag

; --- AH=01h/11h: Keystroke status ---
; ZF=1: no keystroke waiting. ZF=0: AX = the next keystroke, which stays in
; the buffer. Returns with interrupts enabled, as the IBM BIOS does.
.kb_status:
    push ds
    push bx
    push bp
    mov bp, sp               ; [bp+10] = the caller's flags
    mov bx, 0x0040
    mov ds, bx
    cli
    or word [bp+10], 0x0240  ; IF=1, ZF=1
    mov bx, [BDA_KBD_BUF_HEAD]
    cmp bx, [BDA_KBD_BUF_TAIL]
    je .kb_status_done
    mov ax, [bx]
    and word [bp+10], 0xFFBF ; ZF=0
.kb_status_done:
    pop bp
    pop bx
    pop ds
    iret

; --- AH=02h: Shift flags in AL ---
.kb_flags:
    push ds
    push bx
    mov bx, 0x0040
    mov ds, bx
    mov al, [BDA_KBD_FLAGS1]
    pop bx
    pop ds
    iret

; --- AH=12h: Extended shift flags ---
; AL = the shift flags as for AH=02h.
; AH: bit 0 = left Ctrl down, bit 1 = left Alt down, bits 4-6 = Scroll Lock,
;     Num Lock, Caps Lock held down. The keyboard has a single Ctrl and a
;     single Alt key; they are reported as the left ones. Bits 2-3 (right
;     Ctrl/Alt) and bit 7 (SysReq) are always 0.
.kb_eflags:
    push ds
    push bx
    mov bx, 0x0040
    mov ds, bx
    mov al, [BDA_KBD_FLAGS1]
    mov ah, [BDA_KBD_FLAGS2]
    and ah, 0x70             ; the lock keys' "held" bits are in the same place
    test al, 0x04
    jz .kb_eflags_alt
    or ah, 0x01
.kb_eflags_alt:
    test al, 0x08
    jz .kb_eflags_done
    or ah, 0x02
.kb_eflags_done:
    pop bx
    pop ds
    iret
