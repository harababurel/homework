;PRACTICAL EXAM. PUSCAS SERGIU, GROUP 917. 6 JAN 2016
;NOTE: does not write anything to file if it already exists
;      please use different filenames at each execution
;      or delete the output file after running
assume cs:code, ds:data

data segment
    a           dw -50
    b           dw -125
    sol         dw ?
    filename    db 50, ?, 70 dup(0)
    lenfilename dw ?
    handle      dw ?
    msg     db 'suma este '
    lenmsg      db $-msg
    minus       db '-'
    base        dw 10
    digit       db ?
data ends


code segment
proc write_number
    ; writes the representation of `sol`
    ; to the given file

    mov cx, 0               ; this will store the number of digits
    mov ax, sol
    mov dx, 0
    ;cwd

divide:
    div base                ; dx = remainder, ax = quotient

    push dx
    inc cx

    mov dx, 0
    ;cwd

    cmp ax, 0
    jne divide

pop_digits:
    pop ax
    add al, '0'
    mov digit, al

    push cx                 ; save the counter

    mov ah, 40h
    mov bx, handle
    mov cx, 1
    lea dx, digit
    int 21h

    pop cx                  ; restore the counter

    loop pop_digits

    ret
endp write_number

start:
    push data
    pop ds


    mov ah, 0ah
    lea dx, filename        ; read the filename
    int 21h

    mov ah, 0
    mov al, filename+1
    mov lenfilename, ax
    mov si, lenfilename
    add si, 2
    mov filename[si], 0     ; get rid of the newline character


    mov ah, 3dh
    mov al, 2
    lea dx, filename+2      ; open the file
    int 21h

    jc file_not_found
    jmp file_opened

file_not_found:
    mov ah, 3ch
    mov cx, 0
    lea dx, filename+2
    int 21h

    jc final
    mov handle, ax


file_opened:
    mov ah, 42h
    mov bx, handle
    mov cx, 0
    mov dx, 0
    mov al, 0
    int 21h                 ; mov pointer to beginning of file

    mov ah, 40h
    mov bx, handle          ;handle goes here
    mov ch, 0
    mov cl, lenmsg
    lea dx, msg
    int 21h


    mov ax, a               ; store a in ax
    add ax, b               ; a + b

    mov sol, ax
    and ax, 1000000000000000b
    cmp ax, 0
    jne negativ

pozitiv:
    jmp checkpoint

negativ:
    mov ax, sol
    not ax                  ; complement to 1
    add ax, 1               ; (negation + add 1)
    mov sol, ax


    mov ah, 40h
    mov bx, handle          ; write a '-' if negative
    mov ch, 0
    mov cl, 1
    lea dx, minus
    int 21h

    jmp checkpoint

checkpoint:
    call write_number

final:

    mov ax, 4c00h
    int 21h
code ends
end start
