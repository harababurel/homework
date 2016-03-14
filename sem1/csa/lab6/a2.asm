assume cs:code, ds:data

data segment
    n db 8
    res dw 0
data ends

code segment
start:
    mov ax,data
    mov ds,ax

    mov ax,0
    mov dx,0

    bucla:
        inc ax      ;numarul curent (1, 2, 3, etc)

        add dx,ax   ;il adunam in dx

        cmp al,n    ;il comparam pe ax cu n
        jb bucla    ;daca e mai mic, repetam bucla
        jmp sfarsit ;daca nu, incheiem

    sfarsit:
    mov res,dx

    mov ax,4C00h
    int 21h
code ends
end start
