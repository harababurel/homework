;6. Being given a string of words, obtain the string (of bytes) of the digits in base 10 of each word from this string.
;Ex.: being given the string: sir DW 12345, 20778, 4596 the result will be 1, 2, 3, 4, 5, 2, 0, 7, 7, 8, 4, 5, 9, 6.

assume cs:code, ds:data
data segment
    s         dw  12345,20778,4596
    dimS      equ ($-s)/2
    newS      db  14 dup(0)
    stackSize db  0
data ends

code segment
start:
    mov ax,data
    mov ds,ax

    mov si,offset s
    add si,(dimS-1)*2      ;now ds:si should point to the last element of s

    std                    ;the direction flag is 1, so right to left

    mov cx,dimS            ;prepare the loop counter
    jcxz sfarsit           ;if cx is 0, don't execute the loop
    bucla:
        lodsw              ;ax = the current element of s

        imparteli:
            mov dx,0       ;extend ax to a doubleword
            cmp ax,0
         je sfarsitImparteli

            mov bx,10
         div bx            ;now ax=quotient, dx=last digit

         push dx           ;push the last digit onto the stack
            inc stackSize
       jmp imparteli

        sfarsitImparteli:
    loop bucla

    mov ch,0
    mov cl,stackSize       ;prepare the loop counter
    mov si,offset newS
    tiparitoareStiva:
        pop dx

        mov newS[si],dl    ;create the new string
        inc si

        add dl,'0'         ;also print it to stdout
        mov ah,2           ;for debugging
        int 21h
    loop tiparitoareStiva


    sfarsit:
        mov ax,4C00h
        int 21h
code ends
end start

