;PUSCAS SERGIU, 917, 14 OCT 2015, LAB 4, TASK 21 (UNSIGNED)
;x = (a*a/b+b*b)/(2+b)+e
;a-byte; b-word; e-doubleword

assume cs:code, ds:data

data segment
    a db 113
    b dw 3021
    e dd 342342
    x dd ?
    newA dw ?
data ends

code segment
start:
    mov ax,data
    mov ds,ax

    mov ah,0
    mov al,a                ;ax = a, now we can multiply it by itself

    mul a                   ;ax = a*a

    mov dx,0                ;ax:dx = a*a, now we can divide by a word
    div b

    ;now ax = quotient = (a*a)/b
    ;and dx = remainder = (a*a)%b, which is not needed

    mov newA,ax             ;newA = a*a/b (we store it for later use)

    mov dx,0                ;same as lines 19-22
    mov ax,b                ;now ax:dx = b*b
    mul b

    add ax,newA             ;ax:dx = a*a/b + b*b
    adc dx,0                ;      + carry

    mov bx,b                ;b += 2
    inc bx
    inc bx
    mov b,bx

    div b

    ;now ax = quotient = (a*a/b + b*b)/(2+b)
    ;and dx = remainder = whatever

    mov dx,0                ;ax:dx = (a*a/b + b*b)/(b+2)

    add ax,word ptr e       ;now we add e to the least significant half
    adc dx,word ptr e+2     ;and then to the most significant half

    mov word ptr x,ax       ;move the result into x
    mov word ptr x+2,dx

    mov ax,4C00h
    int 21h
code ends
end start
