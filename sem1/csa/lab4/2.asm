ASSUME cs:CODE, ds:DATA

data segment
    a dd 12345678h
    b dd 1111ffffh
    x dd ?
data ends

code segment
start:
    mov ax,data
    mov ds,ax

    ;code starts
    mov ax,word ptr a
    mov dx,word ptr a+2

    sub ax,word ptr b
    sbb dx,word ptr b+2

    mov word ptr x,ax
    mov word ptr x+2,dx
    ;code ends

    mov ax,4C00h
    int 21h
code ends
end start
