assume cs:code, ds:data

data segment
    a dd 1423904929
    mascaL dw 0
    mascaH dw 0
    zero dw 0
    unu dw 0
data ends

code segment
start:
    mov ax,data
    mov ds,ax

    mov mascaL,1
    mov mascaH,1
    mov cx,32

    bucla:

        cmp cx,16
        jb mostSignificant
        jmp leastSignificant

        leastSignificant:
            mov bx,word ptr a       ;jumatatea mai putin semnificativa
            mov ax,mascaL           ;salvam masca in ax
            and bx,ax               ;aplicam masca pentru un singur bit
            shl ax,1                ;mutam bitul mastii cu o pozitie la stanga
            mov mascaL,ax           ;si retinem masca noua
            jmp checkpoint
        mostSignificant:
            mov bx,word ptr a[2]    ;jumatatea mai semnificativa
            mov ax,mascaH           ;salvam masca in ax
            and bx,ax               ;aplicam masca pentru un singur bit
            shl ax,1                ;mutam bitul mastii cu o pozitie la stanga
            mov mascaH,ax           ;si retinem masca noua
            jmp checkpoint

        checkpoint:

        cmp bx,0            ;verificam daca avem bit de 0
        je bitNul           ;daca da, sarim la eticheta bitNul
        jmp bitNenul        ;daca nu, sarim la eticheta bitNenul

        bitNul:
            mov bx,zero     ;incrementam cantitatea de biti nuli
            inc bx
            mov zero,bx
            jmp sfarsitBucla

        bitNenul:
            mov bx,unu      ;aici se ajunge daca nu se intra in bitNul
            inc bx          ;incrementam cantitatea de biti nenuli
            mov unu,bx

        sfarsitBucla:
    loop bucla

    cmp unu,zero
    ja moreUnu
    jmp moreZero

    moreZero:
        shr a,2
        jmp sfarsit

    moreUnu:
        ;fa ceva
        jmp sfarsit

    sfarsit:
    mov ax,4C00h
    int 21h
code ends
end start
