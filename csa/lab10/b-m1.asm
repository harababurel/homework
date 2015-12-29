assume cs:code, ds:data

data segment public
        a    dw 0
        b    dw 0
        c    dw 0
        buff db 10, ?, 10 dup(0)
        ten  db 10
        sol  dw ?
data ends

code segment public
extrn   ComputeExpr: proc           ; this will be imported from the other module
public  ReadInteger

ReadInteger:
        ; reads a string
        ; and computes its integer value
        ; returns the value into ax
        ; does not modify registers, except for ax

        push bx                     ; save the registers
        push cx
        push dx
        push si

        mov ah, 0ah                 ; read the buffer
        lea dx, buff
        int 21h

        mov ax, 0                   ; ax will store the number
        mov ch, 0
        mov cl, buff+1

        mov si, 1

        reconstruct:
            inc si                  ; get the next digit
            mul ten                 ; multiply ax by 10

            mov bl, buff[si]
            sub bl, '0'

            add al, bl
            adc ah, 0
            loop reconstruct

        pop si
        pop dx
        pop cx
        pop bx

        ret

start:
        push data
        pop ds

        call ComputeExpr

        mov ax, 4c00h
        int 21h
code ends
end start
