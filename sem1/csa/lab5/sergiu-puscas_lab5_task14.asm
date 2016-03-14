;PUSCAS SERGIU, GROUP 917, 28 OCT 2015, LAB 5, TASK 14
;The word A is given. Obtain the integer number n represented on the bits 13-15 of A. Then obtain the word B by rotating A n positions to the left.

;Example rundown:
;   a = 1010001011101111b
;   n = 101b = 5
;
;   rotate left by 5 positions, which means that:
;
;   10100 01011101111
;   ^^^^^             ^^^^^
;   this part         comes here
;
;   b =   01011101111 10100
;                     ^like this
;
;   b = 0101110111110100b
;   b = 24052 -> we should get this result

assume cs:code, ds:data
data segment
    a dw 1010001011101111b
    b dw ?
    n db ?
data ends

code segment
start:
    mov ax,data
    mov ds,ax

    mov ax,a                        ;store a in ax
    and ax,1110000000000000b        ;keep only bits 13-15
    shr ax,13                       ;move those bits to the right, so we get the integer value

    mov n,al                        ;we have computed the value of n

    mov ax,a                        ;store a in ax
    mov cl,n                        ;prepare the rotation by n positions
    rol ax,cl                       ;rotate a n positions to the left

    mov b,ax                        ;we have computed the value of b

    mov ax,4c00h
    int 21h
code ends
end start
