assume cs:code, ds:data
data segment
    a dw 4815
    base db ?
    stivaSize db 0
    basePrompt db 0ah,0dh,'baza (z)ece sau (d)oi? $'
    zeceConfirm db 0ah,0dh,'ai ales baza zece. $'
    doiConfirm db 0ah,0dh,'ai ales baza doi. $'
data ends

code segment
start:
    mov ax,data
    mov ds,ax

    baseLoop:                       ;this loop asks for the base
        mov ax,0900h                ;that the user wants for the result
        mov dx,offset basePrompt
        int 21h                     ;print 'baza (z)ece sau (d)oi? $'

        mov ah,01h
        int 21h                     ;read one char from stdin

        cmp al,'z'                  ;if the character is 'z'
        je chosenZece               ;then set the base to 10

        cmp al,'d'                  ;if the character is 'd'
        je chosenDoi                ;then set the base to 2
    jmp baseLoop                    ;repeat the loop if neither 'z' nor 'd'


    chosenZece:
        mov ax,0900h
        mov dx,offset zeceConfirm   ;print 'ai ales baza zece. $'
        int 21h

        mov base,10                 ;set the base to 10
        jmp imparteli               ;go to imparteli


    chosenDoi:
        mov ax,0900h
        mov dx,offset doiConfirm     ;print 'ai ales baza zece. $'
        int 21h

        mov base,2                   ;set the base to 2
        jmp imparteli                ;go to imparteli

    imparteli:
        cmp a,0                      ;if a is 0, then stop the loop
        je prepareStiva              ;and go to prepareStiva

        inc stivaSize                ;increment the number of elements in the stack

        mov dx,0                     ;ax:dx contains a
        mov ax,a

        mov bh,0
        mov bl,base                  ;bx contains the base

        div bx                       ;divide ax:dx by bx

        push dx                      ;dx = remainder, goes on the stack
        mov a,ax                     ;ax = quotient, goes back into a
    jmp imparteli

    prepareStiva:
        mov ch,0
        mov cl,stivaSize

    tiparitoareStiva:
        pop dx                       ;the top of the stack goes into dx
        add dl,'0'                   ;add '0' to the value

        mov ah,02h                   ;print the value of dl
        int 21h
    loop tiparitoareStiva

    sfarsit:
        mov ax,4C00h
        int 21h

code ends
end start
