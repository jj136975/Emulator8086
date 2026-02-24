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
