assume cs:code, ds:data

data segment public
        a dw ?
        b dw ?
        c dw ?
data ends

code segment public
extrn ReadInteger: proc
public ComputeExpr

ComputeExpr:
        ; method calls ReadInteger three times (for a, b, and c)
        ; and computes the value of the expression a * b - c
        ; returns the result into dx:ax
        ; does not modify registers, except for ax and dx

        call ReadInteger
        mov a, ax

        call ReadInteger
        mov b, ax

        call ReadInteger
        mov c, ax

        mov ax, a                   ; ax = a
        mul b                       ; dx:ax = a * b

        sub ax, c
        sbb dx, 0                   ; dx:ax = a * b - c

        ret
code ends
end
