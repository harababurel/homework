ASSUME  cs:code, ds:data
DATA SEGMENT
    a db 180
    alg dw ?
    b db 176
    c dw 288
    x dw ?
DATA ENDS

CODE SEGMENT
START:
    mov ax,data
    mov ds, ax

    ;actual code
    mov  al,a
    cbw
    mov alg,ax

    mov al,b
    cbw

    add ax,alg
    add ax,c

    mov x,ax

    mov ax,4C00h
    int 21h
code ENDS
END start


