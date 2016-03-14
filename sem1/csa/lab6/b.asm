assume cs:code, ds:data

data segment
    res dw 0
    x dw 0
data ends

code segment
start:
    mov ax,data
    mov ds,ax

    mov cx,13
    mov ax,1    ;primul termen
    mov dx,1    ;al doilea termen
    mov bx,2    ;suma primilor 2 termeni


    bucla:
        mov x,ax    ;pe ax il tinem minte

        add ax,dx   ;ax = ax + dx
        mov dx,x    ;dx = vechiul ax

        add bx,ax   ;res = res + noul termen
    loop bucla

    mov res,bx      ;salvam rezultatul

    mov ax,4C00h
    int 21h
code ends
end start
