;PUSCAS SERGIU, GROUP 917, 18 NOV 2015, LAB 6, TASK 10
;A byte string S is given. Obtain in the string D the set of the elements of S.
;Exemple:
;S: 1, 4, 2, 4, 8, 2, 1, 1
;D: 1, 4, 2, 8

assume cs:code, ds:data

data segment
    s db '14248211abcabc'     ;the original string
    l equ $-s           ;the number of elements
    frq db 255 dup(0)   ;the frequency of every ascii character
    d db l dup(0)       ;the final string
data ends

code segment
start:
    mov ax,data
    mov ds,ax

    mov ch,0
    mov cl,l    ;prepare the counter for the loop
    mov di,0    ;di will be the size of the solution string

    jcxz sfarsit
    bucla:
        mov si,cx           ;so we can use this as an index in s[]

        mov dl,s[si-1]      ;print the current
        mov ah,2            ;character of s
        int 21h

        mov bh,0            ;construct a word that can be
        mov bl,s[si-1]      ;moved into si

        mov si,bx           ;si = bx = the current character

        mov frq[si],1       ;the frequency of the current character is 1
    loop bucla


    mov ch,0                ;prepare the counter
    mov cl,255
    buclaNoua:
        mov si,cx           ;cx is our index

        cmp frq[si],1       ;if the frequency is 1
        je addToSol         ;then we add the character to the solution

    loop buclaNoua
    jmp afterAdd            ;so that addToSol is not executed once again


    addToSol:
        mov d[di],cl        ;add the current value to the solution
        inc di              ;and increment the size
        dec cl              ;these two lines simulate the
        jmp buclaNoua       ;"loop buclaNoua" instruction

    afterAdd:

    mov dl,10               ;print a newline character
    mov ah,02h              ;and carriage return
    int 21h
    mov dl,13
    mov ah,02h
    int 21h

    mov cx,di               ;iterate through the solution
    printer:
        mov si,cx

        mov dl,d[si-1]      ;print the current character
        mov ah,2            ;of d
        int 21h
    loop printer

    sfarsit:

    mov ax,4C00h
    int 21h
code ends
end start
