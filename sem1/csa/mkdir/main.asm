assume cs:code, ds:data

data segment
        dirname db 'newdir', 0
        error3  db 'Path not found.$'
        error5  db 'Access denied, or pathname already exists.$'
data ends

code segment
start:
        push data
        pop ds

        lea dx, dirname
        mov ah, 39h
        int 21h

        jnc fin

        cmp ax, 3
        jne access_denied

path_not_found:
        lea dx, error3
        mov ah, 09h
        int 21h
        jmp fin

access_denied:
        lea dx, error5
        mov ah, 09h
        int 21h
        jmp fin

fin:
        mov ax, 4c00h
        int 21h
code ends
end start
