assume cs: code, ds: data

data segment
	dummy db 0Dh, 0Ah, '$'
	string1 db 100, 99 dup ('$')
	string2 db 100, 99 dup ('$')
	msg1 db "first: $"
	msg2 db "second: $"
data ends

code segment

OutInt proc ; функция для вывода чисел со знаком
	push bp ; указатель базы
	mov bp, sp ; указатель стека
	
	mov ax, [bp+4] ; получаем доступ к лежащему в стеке аргументу dx

    ; Проверяем число на знак.
    test ax, ax
    jns oi1
    
    ; Если оно отрицательное, выведем минус и оставим его модуль.
    mov  cx, ax
    mov  ah, 02h
    mov  dl, '-'
    int  21h
    mov  ax, cx
    neg  ax
    ; Количество цифр будем держать в CX.
    oi1:  
    	xor  cx, cx
        mov  bx, 10 ; основание сс. 10
    oi2:
        xor  dx,dx
        div  bx
    ; Делим число на основание сс. В остатке получается последняя цифра.
    ; Сразу выводить её нельзя, поэтому сохраним её в стэке.
        push  dx
        inc  cx
    ; А с частным повторяем то же самое, отделяя от него очередную
    ; цифру справа, пока не останется ноль, что значит, что дальше
    ; слева только нули.
        test  ax, ax
        jnz   oi2

    ; Теперь приступим к выводу.
    mov  ah,  02h
    oi3:
        pop   dx
        ; Извлекаем очередную цифру, переводим её в символ и выводим.
        add  dl, '0'
        int  21h
        ; Повторим ровно столько раз, сколько цифр насчитали.
        loop  oi3

	pop bp
	pop bx ; сохраняем адрес возврата
	push bx ; кладём адрес возврата на место   
    ret
OutInt endp

strcmp proc ; процедура для сравнения строк лексикографически
	; = 0 - строки равны
	; > 0 - первая строка больше
	; < 0 - вторая строка больше

	push bp
	mov bp, sp
	
	mov dx, [bp+4] ; получаем доступ к лежащему в стеке аргументу dx
	mov bx, dx
	mov al, byte ptr [bx+1] ; получаем длину первой строки

	mov dx, [bp+6] ; получаем доступ к второму лежащему в стеке аргументу dx
	mov bx, dx
	mov bl, byte ptr [bx+1]; получаем длину второй строки

	cmp al, bl ; сравниваем длины строк

	je case1 ; eсли равно, переходим к метке case1
	jg case2 ; если больше, то case2
	jl case3 ; если меньше, то case3

	case1:
		mov cl, al
		JMP defualt ; После выполнения ветвления переходим к концу

	case2:
		mov ax, 0
		sub ax, 1
		JMP exit; После выполнения ветвления переходим к концу

	case3:
		mov ax, 1
		JMP exit ; После выполнения ветвления переходим к концу
	defualt:
		xor ch, ch 
		;завершениe ветвления

	mov di, 2
	
	xor ax, ax
	xor bx, bx
	xor dx, dx

	proverka:
		mov bx, [bp+4]
		mov al, byte ptr [bx+di] ; получаем элемент первой строки

		mov bx, [bp+6]
		mov dl, byte ptr [bx+di]; получаем элемент второй строки

		sub ax, dx
		cmp ax, 0 ; сравниваем их
		jnz exit ; если не ноль, то выходим и в ax результат
		inc di
		loop proverka
		
	exit:
		;метка для выхода из цикла

	pop bp
	pop bx ; сохраняем адрес возврата
	
	;mov ax, offset result

	push ax ; кладём возвращаемое значение под адрес возврата
	push bx ; кладём адрес возврата на место
	ret
strcmp endp


print proc
	push bp ; указатель базы
	mov bp, sp ; указатель стека
	
	mov dx, [bp+4] ; получаем доступ к лежащему в стеке аргументу dx
	
	mov ah, 09h
	int 21h

	pop bp
	pop bx ; сохраняем адрес возврата
	push bx ; кладём адрес возврата на место
	ret
print endp


input proc
	push bp ; указатель базы
	mov bp, sp ; указатель стека
	
	mov dx, [bp+4] ; получаем доступ к лежащему в стеке аргументу dx
	
	mov ax, 0
	mov ah, 0Ah
	int 21h
	
	pop bp
	pop bx ; сохраняем адрес возврата
	push bx ; кладём адрес возврата на место
	ret
endp input

print_perevod proc
	mov dx, offset dummy ; перевод строки
	push dx ; передаем в стек сообщение
	call print
	pop dx
	ret
endp print_perevod

start:	mov ax, data
		mov ds, ax

		mov dx, offset msg1
		push dx ; передаем в стек сообщение
		call print
		pop dx

		mov dx, offset string1
		push dx ; передаем в стек строку для ввода
		call input

		call print_perevod ; функция печати перевода строки

		mov dx, offset msg2
		push dx ; передаем в стек сообщение
		call print
		pop dx

		mov dx, offset string2
		push dx ; кладём аргумент в стек
		call input
		call print_perevod ; функция печати перевода строки
		call strcmp
		call OutInt ; на стеке уже лежит dx, поэтому вызыввем просто OutInt

		;конец программы
		mov ah, 4ch
		int 21h
		code ends
		end start