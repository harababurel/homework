;PUSCAS SERGIU, 917, 14 OCT 2015, LAB 4, TASK 21 (SIGNED)
;RESULT DIFFERS BY SOME EXTENT; DON'T KNOW WHY
;x = (a*a/b+b*b)/(2+b)+e
;a-byte; b-word; e-doubleword

assume cs:code, ds:data

data segment
    a db 18
    b dw -7
    e dd -54335436
    x dd ?
    newA dw ?
data ends

code segment
start:
    mov ax,data
    mov ds,ax

    mov al,a
    cbw                     ;ax = a, now we can multiply it by itself

    imul a                  ;ax = a*a

    cwd                     ;ax:dx = a*a, now we can divide by a word
    idiv b

    ;now ax = quotient = (a*a)/b
    ;and dx = remainder = (a*a)%b, which is not needed

    mov newA,ax             ;newA = a*a/b (we store it for later use)

    mov ax,b                ;now ax:dx = b*b
    cwd
    imul b

    add ax,newA             ;ax:dx = a*a/b + b*b
    adc dx,0                ;      + carry

    mov bx,b                ;bx = b
    inc bx
    inc bx                  ;bx = b+2
    mov b,bx                ;b = b+2

    idiv b

    ;now ax = quotient = (a*a/b + b*b)/(2+b)
    ;and dx = remainder = whatever

    cwd                     ;ax:dx = (a*a/b + b*b)/(b+2)

    add ax,word ptr e       ;now we add e to the least significant half
    adc dx,word ptr e+2     ;and then to the most significant half

    mov word ptr x,ax       ;move the result into x
    mov word ptr x+2,dx

    mov ax,4C00h
    int 21h
code ends
end start
