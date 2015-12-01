;6. Being given a string of words, obtain the string (of bytes) of the digits in base 10 of each word from this string.
;Ex.: being given the string: sir DW 12345, 20778, 4596 the result will be 1, 2, 3, 4, 5, 2, 0, 7, 7, 8, 4, 5, 9, 6.

assume cs:code, ds:data
data segment
    s    dw 12345, 20778, 4596, 0
    newS db 14 dup(0)
data ends

code segment
start:
    mov ax,data
    mov ds,ax

    mov si,offset s

    bucla:
        lodsw       ;ax = the current element of s

        cmp ax,0    ;if the element 0 is reached
        je sfarsit  ;then the string is finished

        mov dx,0    ;extend ax to a doubleword




    sfarsit:
        mov ax,4C00h
        int 21h
code ends
end start
