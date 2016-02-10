;PUSCAS SERGIU, 917, 14 OCT 2015, LAB 3, TASK 14
ASSUME CS:CODE, DS:DATA

DATA SEGMENT
    yy dw 132
    d dw -2
    c equ 64        ;constant
    x dw ?
DATA ENDS

CODE SEGMENT
START:
    mov ax,data
    mov ds,ax

    ;code starts
    mov ax,yy       ;ax contains yy
    add ax,yy       ;ax contains yy+yy
    add ax,c        ;ax contains yy+yy+64

    sub ax,d        ;ax contains (yy+yy+64) - d
    sub ax,d        ;ax contains (yy+yy+64) - (d+d)

    mov x,ax        ;x contains the result
    ;code ends

    mov ax,4C00h
    int 21h
code ENDS
END START
