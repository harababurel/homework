        extern printf

        section .data
n:      equ 100000000
three:  dd  3
five:   dd  5
fmt:    db "%ld", 10, 0

        section .text
        global main
main:
        push rbp                        ; set up stack

        mov r8, n                       ; this will be the current number
        mov r9, 0                       ; this will be the solution

        counter:
                mov rdx, 0              ; if rdx is not 0, you get a SIGFPE
                mov rax, r8
                div dword [three]

                cmp rdx, 0              ; check whether the remainder is 0
                jz found

                mov rdx, 0
                mov rax, r8
                div dword [five]

                cmp rdx, 0
                jz found

                back:

                dec r8
                cmp r8, 0
        jnz counter

                                        ; argument order:
        mov rdi, fmt                    ; rdi - the format of printf
        mov rsi, r9                     ; rsi - first argument
                                        ; rdx - second argument
                                        ; rcx - third argument
        mov rax, 0                      ; rax - no xmm used
        call printf


        pop rbp

        mov rbx, 0                      ; normal exit code
        mov rax, 1                      ; process-termination service
        int 0x80                        ; linux kernel service

found:
        ;mov rdi, fmt
        ;mov rsi, r8
        ;mov rax, 0
        ;call printf

        add r9, r8
        jmp back
