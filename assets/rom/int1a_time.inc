; =============================================================================
; INT 1Ah — Time of Day
; =============================================================================
int1a_handler:
    cmp ah, 0x00
    je .tod_read
    cmp ah, 0x01
    je .tod_write
    ; AH=02-07: RTC functions — not available on 8086, return CF=1
    push bp
    mov bp, sp
    or word [bp+6], 0x0001
    pop bp
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
