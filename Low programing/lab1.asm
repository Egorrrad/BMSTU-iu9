;вычислить (𝑎 ∗ 2 − 𝑏/4) ∗ 𝑐 − 𝑑 4
assume CS:code,DS:data

data segment
a dw 20
b dw 6
c dw 3
d dw 6
table DB '0123456789ABCDEF'
stroka10 db "00000",0ah,"$"
stroka16 db "00000h$"

data ends


code segment
start:
mov ax, data
mov ds, ax
mov ax, a
shl ax, 1 ;умножаем на 2
mov bx, b ;кладем в bx b
mov cl, 2
shr bx, cl ;делим на 4 

sub ax, bx ; ax - bx
mov bx, c
mul bx ; ax * c
mov bx, d
sub ax, bx ; ax - bx

mov si, 4
mov cx, 4
mov bl, 10

mov dx, ax

metka:
div bl 
add ah, 48
 mov stroka10[si],ah
 xor ah, ah
 dec si  
 loop metka
       

mov ax, dx
mov si, 4
mov cx, 4
mov bl, 10h

xor ah, ah


metka1:
div bl
cmp ah, 9
jg bukva
add ah, 48
mov stroka16[si], ah
cmp cx, 0
jmp exit
xor ah, ah
dec si  
loop metka1

bukva:
    add ah, 55
    mov stroka16[si], ah
    xor ah, ah
    dec si  
    cmp cx, 0
    jg metka1

exit:

mov ah, 09h
mov dx, offset stroka10
int 21h
mov dx, offset stroka16
int 21h


mov AX,4C00h
int   21h

code ends
end start
