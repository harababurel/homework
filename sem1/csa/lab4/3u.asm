assume cs:code, ds:data

data segment
    a db 113
    b db 79
    c dd 8131
    d db 17
    x dw ?
    r dw ?
data ends

code segment
start:
    mov ax,data
    mov ds,ax

    ;code starts
    mov ah,0
    mov al,a
    mul b               ;ax = a*b

    mov dx,0            ;ax:dx = a*b

    sub ax,word ptr c   ;subtract least significant half
    sbb dx,word ptr c+2 ;subtract most significant half (with borrow)

    ;ax:dx = a*b - c

    mov bl,d            ;need to make b larger (2 bytes)
    mov bh,0            ;so that division executes on ax:dx, not just ax

    div bx              ;divide ax:dx by bx (which is d)

    ;now ax = quotient
    ;and dx = remainder

    mov x,ax            ;quotient
    mov r,dx            ;remainder
    ;code ends

    mov ax,4C00h
    int 21h
code ends
end start
