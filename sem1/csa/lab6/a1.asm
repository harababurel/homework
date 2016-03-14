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
    mov ch,0
    mov cl,n

    jcxz sfarsit
    bucla:
        add ax,cx
    loop bucla

    sfarsit:
    mov res,ax

    mov ax,4C00h
    int 21h
code ends
end start
