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
