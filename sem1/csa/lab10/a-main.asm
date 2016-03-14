assume cs:code, ds:data

data segment public
        s db 100, ?, 100 dup(0)
data ends

code segment public
extrn   printer:proc                ; we will use an external procedure
start:
        push data
        pop ds

        mov ah, 0ah                 ; buffered input
        lea dx, s                   ; load the offset of s
        int 21h                     ; call the interrupt

        ; now we need to count the number of digit characters in s

        mov ch, 0
        mov cl, s+1                 ; cx = the size of s
        mov si, 1                   ; si = (index of first char) - 1
        mov bx, 0                   ; bx = the solution

        iter:
            inc si                  ; si = index of current char
            mov al, s[si]           ; load the current character

            cmp al, '0'             ; if less than '0'
            jl is_not_digit         ; then not a digit

            cmp al, '9'             ; if greater than '9'
            jg is_not_digit         ; then not a digit

            is_digit:               ; if we reach this point, then
            inc bx                  ; the current character is a digit

            is_not_digit:           ; if the character is not a digit
            loop iter               ; repeat the loop

        call printer

        mov ax, 4c00h
        int 21h
code ends
end start
