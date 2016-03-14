assume cs:code, ds:data

    data segment
s       db 200, ?, 200 dup(?)
n       db 4, ?, 4 dup (?)
sPrompt db 's = $'
nPrompt db 'n = $'
lineNo  db ?, ': $'
star    db '*'
newline db 10
ten     db 10
    data ends

code segment
start:
    push data
    pop ds

    lea dx, sPrompt                     ; display the prompt for reading s
    mov ah, 9
    int 21h

    lea dx, s                           ; read s
    mov ah, 10
    int 21h

    mov dl, newline                     ; print a newline
    mov ah, 2
    int 21h

    lea dx, nPrompt                     ; display the prompt for reading n
    mov ah, 9
    int 21h

    lea dx, n                           ; read n
    mov ah, 10
    int 21h

    mov dl, newline                     ; print a newline
    mov ah, 2
    int 21h

    mov al, 0                           ; al will accumulate the integer value of n
    mov si, 2                           ; si will be the index in the string n
    parseN:
        mov cl, n[si]                   ; load a byte from n into cl

        cmp cl, 0dh                     ; the string is done when the byte 0dh is reached
        je afterParseN                  ; if done, go to the next checkpoint

        sub cl, '0'                     ; ascii code to integer value
        mul ten                         ; multiply by 10
        add al, cl                      ; add the digit

        inc si                          ; prepare the next byte
        jmp parseN                      ; repeat the loop

    afterParseN:

    mov n, al                           ; save the integer value of n to the first byte of n

    mov cx, 0
    mov si, 1
    divide:

        inc cx                          ; cx holds the substring index
        push cx                         ; save it for later use

        mov dl, s[si+1]                 ; do not print the line number
        cmp dl, 0dh                     ; if the string has ended
        je afterDivide

        mov ah, 0                       ; ax = the line count
        mov al, cl
        div ten                         ; divide it by 10
        add ah, '0'                     ; convert the remainder to ascii
        mov lineNo, ah                  ; save it in the first byte of lineNo

        lea dx, lineNo                  ; then print the lineNo string
        mov ah, 9
        int 21h

        mov ch, 0                       ; cx = n
        mov cl, n
        iterate:                        ; at each step in the loop
            inc si                      ; go to the next character of s

            mov dl, s[si]               ; if the end of s is reached,
            cmp dl, 0dh                 ; then break the loop
            je afterDivide

            mov ah, 2                   ; otherwise
            int 21h                     ; print the character
        loop iterate                    ; and repeat

        mov dl, newline                 ; after n characters have been printed,
        mov ah, 2                       ; add a newline
        int 21h

        pop cx                          ; restore cx = the substring index
    jmp divide                          ; process the next substring

    afterDivide:

    mov ax, 4c00h
    int 21h
code ends
end start
