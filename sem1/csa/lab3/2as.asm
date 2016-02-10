ASSUME cs:code, ds:data

data SEGMENT
    a db 14
    b db 32
    x db ?
data ENDS

code SEGMENT
start:
    mov ax,data
    mov ds,ax

    ;actual code
    mov al,a
    add al,b
    mov x,al
    ;gata

    mov ax,4C00h
    int 21h
code ENDS
END start
