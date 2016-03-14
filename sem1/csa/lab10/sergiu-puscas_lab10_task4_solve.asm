assume cs:code, ds:data

data segment public
        hex_table db '0123456789ABCDEF'
        base      dw 16
        newline   db 0ah, 0dh, '$'
data ends

code segment public
public solve
proc solve
        ; subroutine prints the decimal representation
        ; of the value stored in ax
        ; does not alter registers
        ; except for ax

        push bx                             ; save registers for later use
        push cx
        push dx

        mov bx, 0                           ; this will be the number of digits

divide:
        mov dx, 0                           ; dx:ax = the current number
        div base                            ; ax = quotient, dx = remainder

        push dx                             ; save the current digit on the stack
        inc bx                              ; increment the number of digits

        cmp ax, 0                           ; if there is something left to divide
        jne divide                          ; repeat the division process

print:
        mov cx, bx                          ; prepare the counter
        lea bx, hex_table                   ; load the translation table
        print_digit:
            pop ax                          ; get a digit from the stack
            xlat                            ; the digit in AL becomes a hex symbol

            mov ah, 02h                     ; print that symbol to stdout
            mov dl, al
            int 21h
        loop print_digit                    ; repeat until no digits left

        mov ah, 09h                         ; current number is done
        lea dx, newline                     ; print a newline
        int 21h


        pop dx                              ; restore the registers
        pop cx
        pop bx
        ret
endp
code ends
end
