assume cs:code, ds:data
data segment
    a dw 1234
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

    baseLoop:
        mov ax,0900h
        mov dx,offset basePrompt
        int 21h

        mov ah,01h
        int 21h

        cmp al,'z'
        je chosenZece

        cmp al,'d'
        je chosenDoi

    jmp baseLoop


    chosenZece:
        mov ax,0900h
        mov dx,offset zeceConfirm
        int 21h

        mov base,10
        jmp imparteli


    chosenDoi:
        mov ax,0900h
        mov dx,offset doiConfirm
        int 21h

        mov base,2
        jmp imparteli

    imparteli:
        cmp a,0
        je tiparitoareStiva

        mov al,stivaSize
        inc al
        mov stivaSize,al

        mov dx,0
        mov ax,a

        mov bh,0
        mov bl,base

        div bx

        push dx
        mov a,ax
    jmp imparteli

    mov ch,0
    mov cl,stivaSize
    tiparitoareStiva:
        pop dx
        add dl,'0'

        mov ah,02h
        int 21h
    loop tiparitoareStiva

    sfarsit:
        mov ax,4c00h
        int 21h

code ends
end start
