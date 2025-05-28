section .text
global func_printc

func_printc:
    mov rax, [rsp + 8]    ; load full int64 argument
    push rax              ; store it on the stack (temporary buffer)

    mov rdi, 1            ; file descriptor: stdout
    mov rsi, rsp          ; pointer to the character
    mov rdx, 1            ; number of bytes to write
    mov rax, 1            ; syscall: write

    syscall
    add rsp, 8            ; clean up stack
    ret
