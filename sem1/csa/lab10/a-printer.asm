assume cs:code, ds:data

data segment public
        digits    db 0
        base      dw 16
        newline   db 10, 13, '$'            ; newline + carriage return
        hex_table db '0123456789ABCDEF'     ; translation table for hexadecimal output
data ends

code segment public
public printer                              ; this label will be visible to other modules
printer:
        push ax                             ; input: bx = the number to be printed on the screen
        push bx                             ; the subprogram prints the number on the screen (in base 16)
        push cx                             ; it does not modify the registers
        push dx                             ; save the registers

        mov ax, bx                          ; we will work on ax from now on
        mov cx, base                        ; when we divide by cx, we want to divide by the base

        get_digits:
            mov dx, 0

            div cx                          ; dx = current digit, ax = the rest of the number

            inc digits                      ; increment the number of digits
            push dx                         ; push the current digit onto the stack

            cmp ax, 0                       ; if the number is not 0
            jne get_digits                  ; we repeat the process

        mov ah, 09h                         ; print a newline
        lea dx, newline                     ; so that the answer doesn't overlap
        int 21h                             ; with the input string

        lea bx, hex_table                   ; pointer to table into bx

        mov ch, 0
        mov cl, digits                      ; prepare the stack unloading
        unload_stack:
            pop ax

            xlat hex_table                  ; converts al to its corresponding base-16 symbol

            mov ah, 02h                     ; character output
            mov dl, al                      ; the character must be in dl
            int 21h                         ; print it to the screen
            loop unload_stack

        pop dx                              ; restore the registers
        pop cx
        pop bx
        pop ax

        ret
code ends
end
