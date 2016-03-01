assume cs:c, ds:d

d segment
        filename   db '1.in', 0
        filehandle dw ?
        e          db 'Some error occurred :($', 0ah, 0dh
        e1         db 'Function number invalid$', 0ah, 0dh
        e2         db 'File not found$', 0ah, 0dh
        e3         db 'Path not found$', 0ah, 0dh
        e4         db 'No handle available$', 0ah, 0dh
        e5         db 'Access denied$', 0ah, 0dh
        e12        db 'Open mode invalid$', 0ah, 0dh
        buff       db 100 dup(?), '$'
d ends


c segment

error1:
        lea dx, e1
        jmp print_error
error2:
        lea dx, e2
        jmp print_error
error3:
        lea dx, e3
        jmp print_error
error4:
        lea dx, e4
        jmp print_error
error5:
        lea dx, e5
        jmp print_error
error12:
        lea dx, e12
        jmp print_error

start:
        push d
        pop ds

        mov ah, 3dh             ; open a file
        mov al, 0h              ; read-only mode
        lea dx, filename
        int 21h

        mov filehandle, ax

        jnc checkpoint

        cmp ax, 1
        je error1
        cmp ax, 2
        je error2
        cmp ax, 3
        je error3
        cmp ax, 4
        je error4
        cmp ax, 5
        je error5
        cmp ax, 12
        je error12
print_error:
        mov ah, 09h
        int 21h


checkpoint:

        mov ah, 3fh
        mov bx, filehandle
        mov cx, 100
        lea dx, buff
        int 21h

        mov cx, ax
        mov si, cx
        dec si

        mov ax,

        bucla:
            dec si

            mov ah, 02h
            mov dl, buff[si]
            int 21h
        loop bucla



        mov ax, 4c00h
        int 21h
c ends
end start
