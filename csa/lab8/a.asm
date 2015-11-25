assume cs:code, ds:data
data segment
    a dw 1234
    base db ?
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

    loop baseLoop


    chosenZece:
        mov ax,0900h
        mov dx,offset zeceConfirm
        int 21h

        mov base,10
        jmp sfarsit

    chosenDoi:
        mov ax,0900h
        mov dx,offset doiConfirm
        int 21h

        mov base,2
        jmp sfarsit

    imparteli:

    sfarsit:
        mov ax,4c00h
        int 21h

code ends
end start
