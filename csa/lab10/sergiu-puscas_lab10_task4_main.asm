;PUSCAS SERGIU, GROUP 917, 4 JAN 2016, LAB 10, TASK 4
;4. A string of numbers is given. Print on the screen the values in base 16.

assume cs:code, ds:data
data segment public
        s         dw 1, 2, 9, 10, 32135, 9250, 16, 234, 2423, 6950, 23429, 0CAFEh, 2989, 2645
        n         db ($-s)/2
data ends

code segment public
extrn solve: proc                           ; import a procedure from the other module
start:
        push data
        pop ds

        mov ch, 0                           ; set up the loop counter
        mov cl, n                           ; that will process each value of the string

        mov si, 0                           ; string index (initially 0)
        get_current_element:
            mov ax, s[si]                   ; load the current value into ax
            add si, 2                       ; and update the index

            call solve                      ; process the number
        loop get_current_element

        mov ax, 4c00h
        int 21h
code ends
end start
