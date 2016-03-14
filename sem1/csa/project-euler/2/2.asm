        extern printf

        section .data
fmt:    db "%ld", 10, 0
a:      dq 1
b:      dq 1
c:      dq 0
sol:    dq 0
n:      dq 4000000

        section .text
        global main
main:
        push rbp

repeat:
        mov r8, [a]
        add r8, [b]             ; r8 = the new fibo term

        cmp r8, [n]             ;if it exceeds n
        jg fin                  ;then go to fin

        mov r10, r8              ;check the parity of rax
        and r10, 1
        cmp r10, 0
        je found
        back:

        mov r9, [b]             ;a <- b
        mov [a], r9
        mov [b], r8             ;b <- rax = the new term


        mov rdi, fmt
        mov rsi, [b]
        mov rax, 0
        call printf

        jmp repeat

found:
        mov rax, [sol]
        add rax, r8
        mov [sol], rax

        mov rdi, fmt
        mov rsi, [b]
        mov rax, 0
        ;call printf
        jmp back

fin:
        mov rdi, fmt
        mov rsi, [sol]
        mov rax, 0
        call printf

        pop rbp
        mov rbx, 0
        mov rax, 1
        int 0x80

