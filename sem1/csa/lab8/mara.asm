;5. Print the current system time on the screen using the hh:mm format and also specify whether it is 'AM' or 'PM'.

assume cs:code, ds:data
data segment
    ;chestii declarate aici
    zece db 10
    primaCifra db ?
    aDouaCifra db ?
data ends

code segment
start:
    mov ax,data
    mov ds,ax

    mov ah,2Ch
    int 21h

    ;Returns:       CH         Hour (0 - 23)
    ;               CL         Minutes (0 - 59)
    ;               DH         Seconds (0 - 59)
    ;               DL         Hundredths of a second (0 - 99)

    cmp ch,10
    jl O_CIFRA

    DOUA_CIFRE:
        ;face ceva
        mov al,ch               ;il bagam pe ch (=ora) in ax
        mov ah,0

        div zece                ;impartim la 10

        ;al = catul             ;catul = prima cifra
        ;ah = restul            ;restul = a doua cifra

        mov primaCifra,al
        mov aDouaCifra,ah

        add primaCifra,'0'      ;transformam cifrele in caractere ASCII
        add aDouaCifra,'0'

        mov ah,02h              ;tiparim prima cifra
        mov dl,primaCifra
        int 21h

        mov ah,02h              ;tiparim a doua cifra
        mov dl,aDouaCifra
        int 21h

        jmp CHECKPOINT


    O_CIFRA:
        ;face altceva
        mov ah,02h
        mov dl,'0'
        int 21h         ;bucata asta tipareste un 0

        add ch,'0'

        mov ah,02h
        mov dl,ch
        int 21h

        jmp CHECKPOINT

    CHECKPOINT:
        mov ah,02h
        mov dl,':'      ;doua puncte intre ora si minut
        int 21h

    cmp cl,10
    jl O_CIFRA_MINUTE

    DOUA_CIFRE_MINUTE:
        mov al,cl
        mov ah,0

        div zece

        ;al = catul
        ;ah = restul

        mov primaCifra,al
        mov aDouaCifra,ah

        add primaCifra,'0'
        add aDouaCifra,'0'

        mov ah,02h
        mov dl,primaCifra
        int 21h

        mov ah,02h
        mov dl,aDouaCifra
        int 21h

        jmp FINISH

    O_CIFRA_MINUTE:
        mov ah,02h
        mov dl,'0'
        int 21h

        add cl,'0'

        mov ah,02h
        mov dl,ch
        int 21h

        jmp FINISH


    FINISH:
        mov ax,4c00h
        int 21h
code ends
end start
