; test_kbd.asm — nasm -f bin test_kbd.asm -o test_kbd.img
cpu 8086
bits 16
org 0x7C00

    ; Print prompt
    mov si, msg
    call print

.loop:
    ; INT 16h AH=00 — wait for keypress
    mov ah, 0x00
    int 0x16

    ; AL = ASCII, AH = scancode
    cmp al, 0x1B           ; ESC to quit
    je .done

    ; Print the character via INT 10h teletype
    mov ah, 0x0E
    xor bx, bx
    int 0x10

    jmp .loop

.done:
    mov si, msg_bye
    call print
    hlt

print:
    lodsb
    cmp al, 0
    je .end
    mov ah, 0x0E
    xor bx, bx
    int 0x10
    jmp print
.end:
    ret

msg:     db 'Type keys (ESC to quit): ', 0
msg_bye: db 13, 10, 'Bye!', 13, 10, 0

times 510-($-$$) db 0
dw 0xAA55