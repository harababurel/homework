ASSUME CS:CODE, DS:DATA

DATA SEGMENT
    A DW 800
    B DB -23
    X DW ?
DATA ENDS

CODE SEGMENT
start:
    MOV AX,DATA
    MOV DS,AX

    MOV AL,B
    CBW
    ADD AX,A

    MOV X,AX

    MOV AX,4C00h
    INT 21h
CODE ENDS
END start
