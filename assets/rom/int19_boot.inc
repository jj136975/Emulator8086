; =============================================================================
; INT 19h — Bootstrap
;
; The emulator supplies the boot order through BOOT_ORDER_PORT: a write
; rewinds the list, each read returns the next BIOS drive number to try, and
; BOOT_ORDER_END is returned once the list is exhausted.
;
; The first sector of each drive is read to 0000:7C00. A hard disk is booted
; when the sector ends in 55AAh, a diskette when the sector looks like code.
; =============================================================================
BOOT_ORDER_PORT equ 0xB1
BOOT_ORDER_END  equ 0xFF

int19_handler:
    sti
    mov al, 'L'
    out 0xE9, al

.restart:
    out BOOT_ORDER_PORT, al     ; rewind the boot-order list

.try_next:
    in al, BOOT_ORDER_PORT
    cmp al, BOOT_ORDER_END
    je .fail
    mov dl, al                  ; DL = drive to try

    xor ax, ax
    mov es, ax
    mov bx, 0x7C00
    mov ax, 0x0201              ; read one sector
    mov cx, 0x0001              ; cylinder 0, sector 1
    xor dh, dh                  ; head 0
    int 0x13
    jc .try_next

    test dl, 0x80
    jz .diskette
    cmp word [es:0x7DFE], 0xAA55    ; a hard disk must carry the boot signature
    jne .try_next
    jmp .boot

.diskette:
    ; Diskettes are older than the signature, so it is not required. The
    ; sector is refused only when it cannot be code: a first byte below 06h,
    ; or a fill pattern (its first nine words all the same).
    cmp byte [es:0x7C00], 0x06
    jb .try_next
    mov ax, [es:0x7C00]
    mov di, 0x7C02
    mov cx, 8
    cld
    repe scasw
    je .try_next

.boot:
    mov al, '!'
    out 0xE9, al
    xor ax, ax
    mov ds, ax
    jmp 0x0000:0x7C00           ; DL = boot drive

.fail:
    mov al, 'F'
    out 0xE9, al
    mov si, msg_fail
    call print_cs
    xor ah, ah                  ; wait for a key, then try again
    int 0x16
    jmp .restart
