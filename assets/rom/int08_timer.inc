; =============================================================================
; INT 08h — Timer Tick (IRQ0)
; =============================================================================
int08_handler:
    push ds
    push ax
    mov ax, 0x0040
    mov ds, ax

    ; Increment tick counter
    inc word [BDA_TIMER_LOW]
    jnz .no_hi
    inc word [BDA_TIMER_HIGH]
.no_hi:
    ; 24-hour rollover: 0x180B0 ticks = 1573040 / 65536 = 0x0018:0x00B0
    cmp word [BDA_TIMER_HIGH], 0x0018
    jb .no_roll
    cmp word [BDA_TIMER_LOW], 0x00B0
    jb .no_roll
    mov word [BDA_TIMER_HIGH], 0
    mov word [BDA_TIMER_LOW], 0
    mov byte [BDA_TIMER_OFLOW], 1
.no_roll:

    ; Diskette motor timeout countdown
    mov al, [BDA_MOTOR_TIMEOUT]
    or al, al
    jz .no_motor
    dec al
    mov [BDA_MOTOR_TIMEOUT], al
    jnz .no_motor
    ; Motor off — write 0x0C to FDC DOR (enable DMA/IRQ, no motors)
    push dx
    mov dx, 0x3F2
    mov al, 0x0C
    out dx, al
    pop dx
    mov byte [BDA_MOTOR_STATUS], 0
.no_motor:

    ; Send EOI to PIC (before INT 1Ch so other IRQs aren't blocked)
    mov al, 0x20
    out 0x20, al

    ; Call user timer hook (INT 1Ch)
    int 0x1C

    pop ax
    pop ds
    iret
