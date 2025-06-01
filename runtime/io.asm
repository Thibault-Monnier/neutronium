section .text
global func_print_c
global func_print_num

; ------------------------------------------------
; fn print_c(char: int);
; ------------------------------------------------
func_print_c:
    mov rax, [rsp + 8]    ; load full int64 argument
    push rax              ; store it on the stack (temporary buffer)

    mov rdi, 1            ; file descriptor: stdout
    mov rsi, rsp          ; pointer to the character
    mov rdx, 1            ; number of bytes to write
    mov rax, 1            ; syscall: write

    syscall
    add rsp, 8            ; clean up stack
    ret


; ------------------------------------------------
; fn print_num(num: int);
; ------------------------------------------------
func_print_num:
    push    rbp
    mov     rbp, rsp

    mov     rax, [rbp + 16]     ; rax ← the int64 we want to print

    ; - Allocate 8 bytes to store "current quotient" across iterations -
    sub     rsp, 8
    mov     [rbp - 8], rax     ; store initial value

.loop_extract:
    mov     rax, [rbp - 8]     ; reload “current” value
    mov     rbx, 10
    cqo                        ; sign‐extend rax → rdx:rax
    idiv    rbx                ; rax = quotient, rdx = remainder
    mov     [rbp - 8], rax     ; save new quotient

    add     rdx, '0'           ; remainder → ASCII digit
    push    rdx                ; push the ASCII digit onto stack

    ; If quotient ≠ 0, keep extracting:
    mov     rax, [rbp - 8]
    test    rax, rax
    jnz     .loop_extract

    ;— Now all digit‐bytes are on the stack (LSB first).
    ;   We pop them so the first POP yields the most significant digit. —
.print_digits:
    lea     rdx, [rbp - 8]
    cmp     rsp, rdx
    je      .done_printing

    pop     rdx                ; get ASCII digit
    push    rdx
    call    func_print_c
    pop     rdx
    jmp     .print_digits

.done_printing:
    xor     rax, rax           ; return 0
    leave
    ret