assume cs:code, ds:data

data segment
    a dw 1010111001101100b
    b dw 0111110100100110b
    c dw ?
data ends


code segment
start:
    mov ax,data
    mov ds,ax

    ;bit 0 and 2 of word C are the same as the bits 0 and 1 from word A;
    ;bit 1 of word C is the same as bit 1 of word B
    ;bits 3-5 of word C take the binary value 010
    ;bits 6-9 of word C are the same as bits 11-14 from word A
    ;bits 10-15 of word C take the invert value of bits 3-8 of word B

    ;bit 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
    ;c =!b8 !b7 !b6 !b5 !b4 !b3 a14 a13 a12 a11   0   1   0  a2  b1  a0

    mov dx,0                          ;dx is where we will construct the value of c
    mov ax,a                          ;ax is where we alter the bits needed

    and ax,101b                       ;this keeps bits 0 and 2
    or dx,ax                          ;move those bits into dx

    mov ax,b
    and ax,10b                        ;this keeps bit 1
    or dx,ax                          ;move that bit into bit 1

    ;bits 3 and 5 are already 0, so nothing to be done
    or dx,10000b                      ;put a 1 on position 4

    mov ax,a                          ;again, store a in ax
    mov cl,5                          ;prepare the 5-pos right shift
    shr ax,cl                         ;apply the shift on ax
    and ax,0000001111000000b          ;and only keep bits 6-9
    or dx,ax                          ;then apply them on dx

    mov ax,b                          ;store b in ax
    not ax                            ;flip its bits
    mov cl,7                          ;prepare the 7-pos left shift
    shl ax,cl                         ;apply the shift on ax
    and ax,1111110000000000b          ;and only keep bits 10-15
    or dx,ax                          ;then apply them on dx

    mov c,dx                          ;move the result to variable c

    mov ax,4c00h
    int 21h
code ends
end start
