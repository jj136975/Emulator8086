; =============================================================================
; INT 08h — Timer Tick (IRQ0)
;
; The order of events is the IBM BIOS's: interrupts on, time-of-day and
; diskette motor bookkeeping, the user hook INT 1Ch, then the end-of-interrupt.
; IRQ0 is still in service while the hook runs, and it has the highest
; priority, so the interrupt controller holds every request back until the
; hook has returned: a hook that enables interrupts is not interrupted by
; another device, nor entered again by the next tick.
; =============================================================================
int08_handler:
    sti
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

    ; User timer hook
    int 0x1C

    ; End of interrupt. Interrupts stay off from here to the IRET (as in the
    ; PC/AT BIOS), so a tick that is already pending is taken once this
    ; handler's stack frame is gone, not on top of it.
    cli
    mov al, 0x20
    out 0x20, al

    pop ax
    pop ds
    iret
