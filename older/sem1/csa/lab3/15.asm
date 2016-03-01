;SAVAN MARA, 917, 14 OCT 2015, LAB 3, TASK 15
ASSUME CS:CODE, DS:DATA
data segment
    a EQU 129
    h dw 200
    d db 8
    m db 23
    x dw ?
data ENDS


code SEGMENT
start:
    mov ax,data
    mov ds,ax

    ;actual code
    mov ax,h        ;ax contains h
    add ax,a        ;ax contains (129+h)

    mov dx,ax       ;dx contains (129+h)

    mov al,d        ;al contains d
    cbw             ;ax contains d

    sub dx,ax       ;dx contains (129+h)-d

    mov al,m        ;al contains m
    cbw             ;ax contains m

    sub dx,ax       ;dx contains (129+h)-(d+m)

    mov x,dx
    ;Gathya its oke its alright

    mov ax,4C00h
    int 21h
code ENDS
END START
