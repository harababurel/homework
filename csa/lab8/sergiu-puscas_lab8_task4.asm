;PUSCAS SERGIU, GROUP 917, 1 DEC 2015, LAB 8, TASK 4
;Print the current date on the screen and also the current day of the week (using letters, not numbers).

assume cs:code, ds:data

data segment
    mon         db ' Monday$'
    tue         db ' Tuesday$'
    wed         db ' Wednesday$'
    thu         db ' Thursday$'
    fri         db ' Friday$'
    sat         db ' Saturday$'
    sun         db ' Sunday$'
    day         db ?
    weekday     db ?
    zece        db 10
data ends

code segment
start:
    mov ax,data
    mov ds,ax

    mov ah,42   ;Get System Date
    int 21h

    ;Now we have:
    ;
    ;AL         Day of the week (0 - 6; 0 = Sunday)
    ;CX         Year (1980 - 2099)
    ;DH         Month (1 - 12)
    ;DL         Day (1 - 31)

    mov day,dl              ;store the current day into <day>
    mov weekday,al          ;and the weekday into <weekday>

    printFirstDigit:
        mov ah,0            ;move the day into ax in order to perform arithmetic operations
        mov al,day          ;divide the day by 10
        div zece            ;now the concatenation of al+ah gives the exact day of the month

        cmp al,0            ;if the first digit is 0, then don't print it
        je printSecondDigit ;go directly to the second digit

        mov dl,al           ;otherwise, move the first digit of the day into dl
        add dl,'0'          ;and transform it into an ASCII code

        mov ah,2            ;then print it
        int 21h             ;to stdout

    ;first digit should now be printed (if not 0)

    printSecondDigit:
        mov ah,0            ;load the day of the month again
        mov al,day          ;because the ah register has
        div zece            ;changed in the meantime

        mov dl,ah           ;the remainder (=second digit) is stored in ah
        add dl,'0'          ;move it to dl, convert it to ASCII

        mov ah,2            ;and print it
        int 21h

    ;second digit should now be printed

    cmp weekday,0
    je sunday

    cmp weekday,1
    je monday

    cmp weekday,2
    je tuesday

    cmp weekday,3
    je wednesday

    cmp weekday,4
    je thursday

    cmp weekday,5
    je friday

    cmp weekday,6
    je saturday

    jmp sfarsit             ;if, by any chance, the code is not in [0, 6], don't print anything

    sunday:
        mov dx,offset sun   ;load the sunday string
        jmp sfarsit

    monday:
        mov dx,offset mon
        jmp sfarsit

    tuesday:
        mov dx,offset tue
        jmp sfarsit

    wednesday:
        mov dx,offset wed
        jmp sfarsit

    thursday:
        mov dx,offset thu
        jmp sfarsit

    friday:
        mov dx,offset fri
        jmp sfarsit

    saturday:
        mov dx,offset sat
        jmp sfarsit

    sfarsit:
        mov ah,9            ;print the loaded string
        int 21h

        mov ax,4C00h
        int 21h
code ends
end start
