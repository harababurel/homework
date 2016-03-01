assume cs:code, ds:data

data segment
    s db 11 dup(0)
data ends

code segment
start:
    mov ax,data
    mov ds,ax

    mov si,offset s     ;the first byte contains
    mov s[si],10        ;the maximum number of characters

    mov dx,offset s
    mov ah,0Ah          ;buffered input
    int 21h


    mov ax,4c00h
    int 21h
code ends
end start
