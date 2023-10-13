;найти скалярное произведение векторов
assume CS:code,DS:data

data segment	 
    mas dw 1, 2, 3, 4, 5
    mas2 dw 1, 4, 3, 2, 1
	len dw 4
    stroka10 db "0000000",0ah,"$"
data ends


code segment
start:
mov ax, data
mov ds, ax



mov si, 0
mov cx, 5
mov bx, 0

scalar:
    mov ax, word ptr mas[si]
    mul word ptr mas2[si]
    add bx, ax
    add bx, dx
    add si, 2
    loop scalar

mov ax, bx
mov si, 6
mov cx, 6
mov bl, 10


metka:
    div bl 
    add ah, 48
    mov stroka10[si],ah
    xor ah, ah
    dec si  
    loop metka
       

mov ah, 09h
mov dx, offset stroka10
int 21h


mov AX,4C00h
int   21h

code ends
end start