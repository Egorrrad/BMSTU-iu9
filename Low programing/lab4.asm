assume cs: code, ds: data

data segment

; не более 10 символов в строке
; также первый символ строки это знак
; он будет учитываться при знаковом сложении и умножении

num1 db 12, 12 dup('$')
num2 db 12, 12  dup('$')

num1_dest db 12, 12 dup('$')
num2_dest db 12, 12  dup('$')


s3 db 12, 12  dup('$')



first_pos_num1 dw 1 ; нужно чтобы не учитывать знак минуса при знаковых операциях
first_pos_num2 dw 1
first_pos_res dw 1

max_len equ 12
lenres equ 22
res db lenres, lenres, lenres dup ('$'), 10, '$'

unires db 11, 11 dup (0), "$"
temp db 11, 11 dup (0), "$"

dummy db 'here', 10, '$'

newline db 10, 36, 36
space db ' ', '$'

divider db 10
beznak db 0

from_plus db 0

input1 db 10, 'input the first number', 10, 36
input2 db 10, 'input the second number', 10, 36
rules db 10, 'input only 10 digits', 10, 36

plus_str db 'sum: $'
minus_str db 'difference : $'
mult_str db 'multipli: $'

mode10_str db 10, 'in 10 mode', 10, '$'
mode16_str db 10, 'in 16 mode', 10, '$'

errormsg db 10, 'INVALID SYMBOL:$'
fatal_erorrmsg db 10, 'ERROR$'

num db "00000", 10, '$'

data ends

sseg segment stack
db 256 dup(0)
sseg ends

code segment


coppy_str proc
	push si
    push ax
    push di
	; Копирование строки
	loop_copy:
		; Загрузка текущего байта из исходной строки в регистр al
		mov al, [si]
		
		; Сохранение текущего байта в целевой строке
		mov [di], al
		
		; Увеличение адресов исходной и целевой строк
		inc si
		inc di
		
		; Проверка на конец строки (символ '\0')
		cmp al, 0
		jne loop_copy

	; Конец копирования строки

	; Здесь можно выполнить дополнительные действия, если нужно
	pop di
    pop ax
    pop si
	ret
coppy_str endp

swap_nums proc
    push si
    push ax
    push bx
    mov si, max_len 
    dec si
    loop_swap:
        mov al, num1[si]
        mov bl, num2[si]
        mov num1[si], bl
        mov num2[si], al

        dec si
        cmp si, 0
        je break_swap
        jmp loop_swap
    break_swap:    
    pop bx
    pop ax
    pop si
    ret
swap_nums endp

fatal_error proc
	push bx dx si ax
			mov ah, 09h
            lea dx, fatal_erorrmsg
            int 21h
			
            mov ah, 4ch 
            int 21h ; вызов прерывания для выхода из программы

	pop ax si dx bx
	ret
fatal_error endp

print_ax proc   
    push bx dx si ax

    mov bx, 10
    mov si, 4
    xor dx, dx

    dec_label:
        div bx
        add dx, 48
        mov num[si], dl

        xor dx, dx
        dec si
        cmp ax, 0

        jg dec_label


    mov ah, 09h
    mov dx, offset num
    int 21h

    mov si, 0
    mov num[si], 48
    mov si, 1
    mov num[si], 48
    mov si, 2
    mov num[si], 48
    mov si, 3
    mov num[si], 48
    mov si, 4
    mov num[si], 48

    pop ax si dx bx
    
    ret
print_ax endp

print_newline proc
	push ax dx
	mov dx, offset newline
	mov ah, 09h
	int 21h
	pop dx ax
	ret
print_newline endp

print_space proc
	push ax dx
	mov dx, offset space
	mov ah, 09h
	int 21h
	pop dx ax
	ret
print_space endp

print_here proc
	push dx 
	push ax
	mov dx, offset dummy
	mov ah, 09h
	int 21h
	pop ax
	pop dx
	ret
print_here endp

put_zero_in_res_foo proc
    push cx bx

    mov cx, lenres
    mov bx, 2
    md_put_zero:
        mov res[bx], 0
        inc bx
        loop md_put_zero

    pop bx cx
    ret
put_zero_in_res_foo endp

; взвращает 1 в ax, если num1>num2, 
; 2, если num2>num1
; 0, если num1=num2

num1_bigger_num2_foo proc  
    push bx cx dx si di

    mov si, 1
    ; сравниваем длинну строк
    mov al, num1[1]
    mov bl, num2[1]
    cmp al, bl
    jg num1_greater
    jl num2_greater

	


    xor ch, ch
    mov cl, num1[1]  ; положили в cx длинну строк
    inc cl         ;(они одинаковые по длинне)

    eq_len:
        inc si 
        mov al, num1[si]
        mov bl, num2[si]
        cmp al, bl
        jg num1_greater
        jl num2_greater
        loop eq_len
        mov ax, 0
    
    num1_greater:
        mov ax, 1
        jmp cmpend
    num2_greater:
        mov ax, 2
        jmp cmpend

    cmpend:

    pop di si dx cx bx
	ret

num1_bigger_num2_foo endp

; непонятные inc`и индексных регистров связаны
; с тем, что в начале строки хранятся 2 
; байта: с массимальной и реальной длинной

mul_foo proc
    push ax bx cx dx si di

    ; зануляем строку res
    call put_zero_in_res_foo

    mov first_pos_num1[0], 1
    mov first_pos_num2[0], 1

	
	;теперь надо сравнить знаки
	mov al, num1[2]
    mov bl, num2[2]

	cmp al, '-'
	je proverka_bl

	cmp bl, '-'
	je bl_mul_with_minus

	jmp go_to_beznak_mul

	proverka_bl:
		
		cmp bl, '-' ; сравниваем с минусом второе или нет
		je al_bl_mul_withminus
		jmp al_mul_with_minus



	al_bl_mul_withminus:
		; выполняем обычное умножение и убираем минусы
		;mov num1[2], 0
		;mov num2[2], 0
		mov first_pos_num1[0], 2 ; просто увеличим первую позицию для числа
		mov first_pos_num2[0], 2 ; просто увеличим первую позицию для числа

		jmp go_to_beznak_mul


	al_mul_with_minus:
		
		; получается первый множитель с минусом
		; значит все произведение с минусом должно быть
		
		;mov num1[2], 0 ; уберем знак минуса 
		mov first_pos_num1[0], 2
		mov res[2], '-' ; поставим его в начале результата

		; выполним просто умножение
		jmp go_to_beznak_mul

	bl_mul_with_minus:
		; тут второй множитель отрицательный
		; уберем у него знак и в результат запишем
		;mov num2[2], 0
		mov first_pos_num2[0], 2
		mov res[2], '-'

		jmp go_to_beznak_mul

	go_to_beznak_mul:
		;jmp dm_end

	

    ; di - на конец num2
    xor ax, ax
    mov al, num2[1]
    inc al
    mov di, ax

    dm_multing:
     
        ; подготавливаем помножение на разряд

        ; переводим si в конец num1
        xor ax, ax
        mov al, num1[1]
        inc al
        mov si, ax

        ; 0 в уме
        xor dl, dl

        ; помножаем на разряд
        mov dh, num2[di] ; в dh кладем разряд num2, на который будем помножать num1
        dm_multing_digit: 


            mov al, [si] ; записали в al тот разряд num1, который будем помножать
            mul dh  ; теперь в ax результат перемножения 
                    ; но на самом деле в al - потому что число маленькое
            add al, dl ; прибавили то, что в уме
            xor dl, dl ; и сразу же забыли
            div divider ; теперь в ah искомый разряд

            mov dl, al ; результат  держим в уме (в dl)
            
            ; теперь добавим к res результат перемножения из ah
            xor cx, cx ; в cx запишем сумму длинн строк
            add cl, num1[1]
            add cl, num2[1]
            sub cx, si ; и вычтем длинну остатка строк
            inc cx
            sub cx, di ; тем самым, получим разряд res,
                        ; в который надо добавить ah
            inc cx 
            mov bl, res[1]  ; в bx записали дилинну res
            inc bl
            sub bl, cl      ; и вычитаем из нее длинну остатка
            mov cl, res[bx]
            add cl, ah
            mov al, cl
            xor ah, ah

            div divider
            add dl, al
            ; в al результат деления (т.е. 1, если был переход через разряд)
            ; а в ah - цифра, которую надо записать
            
            mov res[bx], ah
            xor ax, ax
            mov al, res[bx]

            dec si ; сдвинулись влево по первой строчке
            cmp si, first_pos_num1 ; если совпадает с началом, заканчиваем
            jg dm_multing_digit ; если не дошли до конца si
        
        ; если в уме что-то осталось, запишем это
        ; в самы старший разряд
        ; причем, он гарантированно нулевой, поэтому просто записываем
        dec bx ; - вот это разряд в res
		

        mov res[bx], dl
        
        dec di ; сдинулись вправо по второму множителю
        cmp di , first_pos_num2
        je dm_end
        jmp dm_multing

    dm_end:
        
    pop di si dx cx bx ax
    ret
mul_foo endp

; res = num1 + num2
plus_operate proc
    push ax bx cx dx si di

	mov si, offset num1 ; помещаем адрес исходной строки в регистр si
    mov di, offset num1_dest ; помещаем адрес целевой строки в регистр di
    mov cx, 11 ; количество байт для копирования (длина строки)
    cld ; устанавливаем флаг направления вперед для movsb

    rep movsb ; копируем ecx байтов из исходной строки в целевую строку

	mov si, offset num2 ; помещаем адрес исходной строки в регистр si
    mov di, offset num2_dest ; помещаем адрес целевой строки в регистр di
    mov cx, 11 ; количество байт для копирования (длина строки)
    cld ; устанавливаем флаг направления вперед для movsb

    rep movsb ; копируем ecx байтов из исходной строки в целевую строку


    ; зануляем строку res
    call put_zero_in_res_foo


	;теперь надо сравнить знаки
	mov al, num1_dest[2]
    mov bl, num2_dest[2]
	
	cmp al, '-'
	je prov_bl

	cmp bl, '-'
	je bl_with_minus

	jmp exit_to_plus

	prov_bl:
		cmp bl, '-' ; сравниваем с минусом второе или нет
		je al_bl_with_minus
		jmp al_with_minus




	al_bl_with_minus:
		; выполняем сложение и ставим минус вначале
		mov res[2], '-'
		; увеличиваем первую позицию для числе чтобы не складывать код минуса
		; у двух чисел

		mov first_pos_num1[0], 2
		mov first_pos_num2[0], 2

		mov num1_dest[2], 0
		mov num2_dest[2], 0

		jmp exit_to_plus


	al_with_minus: ; не работает совсем
		
		;mov num1_dest[2], 0 ; нельзя занулять иначе другие операции некорректно будут

		mov first_pos_num1[0], 2 ; просто увеличим первую позицию для числа

		

		call swap_nums

		mov from_plus[0], 1 ; обозначим что пришли от сложения
		; нужно чтобы не копировать заново значение массива
		call minus_foo
		jmp exit_sum


	bl_with_minus: ; работает!!!!
		;mov num2_dest[2], 0 тоже нельзя занулять иначе другие операции некорректно будут

		mov first_pos_num2[0], 2 ; просто увеличим первую позицию для числа
		
		mov from_plus[0], 1 ; обозначим что пришли от сложения
		call minus_foo
		jmp exit_sum


	exit_to_plus:


    xor ax, ax
    ; переводим si на конец num1
    mov al, num1_dest[1]
    inc al
    mov si, ax
    ; di - на конец num2
    mov al, num2_dest[1]
    inc al
    mov di, ax
    ; bx - на конец res
    mov al, res[1]
    inc al
    mov bx, ax

    xor dl, dl
    dp_adding:
        mov al, num1_dest[si]
        mov ah, num2_dest[di]
        
		; обнуляем если дошли до минуса

		cmp al, '-'
		je zero_al
		
		return_from_al:

		cmp bl, '-'
		je zero_bl

		return_from_bl:

        add al, ah
        add al, dl
		
		jmp go_to_adding 

		zero_al:
			mov al, 0
			jmp return_from_al

		zero_bl:
			mov bl, 0
			jmp return_from_bl

		go_to_adding:

		; конец обнуления

        xor ah,ah
        div divider
        
        mov dl, al  ; в al результат деления
                    ; запоминаем его в уме
        			; а в ah - цифра, которую надо записать
        mov res[bx], ah        

        dec si
        dec di
        dec bx

        ; по умолчанию, первой должна закончиться num2

		
        cmp si, first_pos_num1 ; если кончилась si по умолчанию до 1
        je dp_num1_ended
		
        cmp di, first_pos_num2 ; если кончилась di по умолчанию до 1
        je dp_num2_ended


        jmp dp_adding

    dp_num1_ended: ; если закончислась num1
        mov cx, di  ; записываем в cx,
        ; записываем в si указатель на текущий байт в num2
        lea si, num2_dest
        add si, cx
        dec cx      ; сколько раз осталось "копировать" из num2 в res
        jmp pre_dp_end_adding

    dp_num2_ended: ; если закончислась num2
        mov cx, si  ; записываем в cx,
        ; записываем в si указатель на текущий байт в num1
        lea si, num1_dest
        add si, cx
        dec cx      ; сколько раз осталось "копировать" из num1 в res
    pre_dp_end_adding:
        cmp cx, 0
        je dp_end
    dp_end_adding:        
        mov al, [si]
		cmp al, '-'
		je go_zero
		jmp exit3
		go_zero:
		 	mov al, 0
		exit3:

		add al, dl
		
        xor ah,ah
        div divider
        mov dl, al  ; в al результат деления
                    ; запоминаем его в уме
        ; а в ah - цифра, которую надо записать
        mov res[bx], ah 

        dec bx
        dec si
        
        loop dp_end_adding
    
    dp_end:
    mov res[bx], dl ; не забываем перенести все из ума
        
	exit_sum:


    pop di si dx cx bx ax
    ret
plus_operate endp

minus_foo proc
    push ax bx cx dx si di

	;cmp from_plus, 1
	;je go_to_operation

	mov si, offset num1 ; помещаем адрес исходной строки в регистр esi
    mov di, offset num1_dest ; помещаем адрес целевой строки в регистр edi
    mov cx, 11 ; количество байт для копирования (длина строки)
    cld ; устанавливаем флаг направления вперед для movsb

    rep movsb ; копируем ecx байтов из исходной строки в целевую строку

	mov si, offset num2 ; помещаем адрес исходной строки в регистр esi
    mov di, offset num2_dest ; помещаем адрес целевой строки в регистр edi
    mov cx, 11 ; количество байт для копирования (длина строки)
    cld ; устанавливаем флаг направления вперед для movsb

    rep movsb ; копируем ecx байтов из исходной строки в целевую строку

	cmp num1_dest[2], '-'
	je zero_num1
	jmp to_num2
	zero_num1:
		mov num1_dest[2], 0
	to_num2:

	cmp num2_dest[2], '-'
	je zero_num2
	jmp do_minus
	zero_num2:
		mov num2_dest[2], 0

	do_minus:


    ; зануляем строки для ответов
    call put_zero_in_res_foo
    
    xor ax, ax
    ; переводим si на конец num1
    lea si, num1_dest
    mov al, num1_dest[1]
    inc al
    add si, ax
    ; di - на конец num2
    lea di, num2_dest
    mov al, num2_dest[1]
    inc al
    add di, ax
    ; bx - на конец res
    mov al, res[1]
    inc al
    mov bx, ax                                    
                                                        
    xor dl, dl ; занулили dl                         
    ; - в нем будем запоминать единичку                                               
    
    ; сравниваем уменьшаемое и вычитаемое
	

    call num1_bigger_num2_foo
    cmp ax, 1
     
    je dm_num1_greater ; если уменьшаемое больше
    jg dm_num2_greater ; если вычитаемое больше
    
	

    ; если числа равны, сразу печатем нули
    pop di si dx cx bx ax
    ret
    ; иначе вычитаем

    xor dx, dx

    ; по умолчанию вычитаем di из si
    dm_num1_greater: ; num1 больше num2
        ; кладем начало длинной строки на стек
        lea cx, num1_dest
        inc cx
        push cx
        ; в cx запоминаем указатель на начало короткой строки
        lea cx, num2_dest
        inc cx
        jmp dm_subbing1
    dm_num2_greater: ; num2 больше num1
        ; поэтому тут меняем их местами 
        xchg si, di
        ; кладем начало длинной строки на стек
        lea cx, num2_dest
        inc cx
        push cx
        ; в cx запоминаем указатель на начало короткой строки
        lea cx, num1_dest
        inc cx
        mov res[2], '-'
    
    dm_subbing1:
        mov al, [si] ; al - уменьшаемое
        mov ah, [di] ; ah - вычитаемое
        add ah, dl  ; то, что было в уме, прибавили к вычитаемому
         
        add al, divider ; заняли единичку (в любом случае)
         
        sub al, ah          ; в al результат вычитания
         
        xor ah, ah ; чтобы делить al, а не ax
        
        div divider     ; и поделим результат на 10
        ; в al результат деления - количество десятков
        ; в ah остаток - нужный нам разряд
        mov dl, al      
        xor dl, 1       
        ; если нам не понадобится то,
        ; что мы занимали, оно окажется в dl
        ; т.е. если не понадобилось, зн в dl 1,
        ; значит мы не занимали, 
        ; значит в dl должен быть 0
        ; поэтому xor`им
        ; аналогично с 0

        mov res[bx], ah ; записали результат в res
        
        ; сдвинулись на единичку влево 
        dec si
        dec di
        dec bx
        
        ; если вычитаемое не закончилось
        cmp di, cx
        jne dm_subbing1

    pop cx ; сняли указатель на начало уменьшаемого
    
    dm_subbing2:
        ; если уменьшаемое закончилось
        cmp si, cx
        je dmm_end

        mov al, [si] ; al - уменьшаемое
        mov ah, dl
        
        add al, divider ; заняли единичку (в любом случае)
        sub al, ah          ; в al результат вычитания
        xor ah, ah
        div divider     ; и поделим результат на 10
        ; в al результат деления - количество десятков
        ; в ah остаток - нужный нам разряд
        mov dl, al      
        xor dl, 1       
        mov res[bx], ah ; записали результат в res
        
        ; сдвинулись на единичку влево 
        dec si
        dec bx
        
        jmp dm_subbing2
        

    ; уменьшаемое гарантированно больше вычитаемого
    ; поэтому в конце dl точно равен 0 
    dmm_end:
    pop di si dx cx bx ax

	ret
minus_foo endp

; переводит строку цифр в массив цифр
; работает с аргументом из регистра si
string_to_num proc
    push ax bx cx dx

    xor ch, ch
    xor ah, ah

    inc si
    mov cl, [si]
	
	mov bl, [si+1]

	; проверка знака
	cmp bl, '-'
	je with_minus
	jmp stn_label

	with_minus:
		inc si
		mov ah, 09h
		lea dx, newline
		int 21h

		
	;конец тладки проверки знака

    stn_label:
        inc si
        mov al, [si]
        xor ah, ah
        
        cmp divider, 10
        je dec_only

        cmp al, 104 ; h
        jg stn_label_err
        cmp al, 97 ; a
        jge stn_laz

        cmp al, 72 ; H
        jg stn_label_err
        cmp al, 65 ; A
        jge stn_uAZ

        dec_only:
			cmp al, 57 ; 9
			jg stn_label_err
			cmp al, 48 ; 0
			jge stn_09

        stn_laz:
            sub al, 87
            jmp stn_label_end
        stn_uAZ:
            sub al, 55
            jmp stn_label_end
        stn_09:
            sub al, 48
            jmp stn_label_end

        stn_label_err:
            mov ah, 09h
            lea dx, errormsg
            int 21h

			call print_space ; вывод пробела

			mov ah, 2 ; установка функции для вывода символа
            mov dl, [si] ; вывод некорректного символа
            int 21h
			
            mov ah, 4ch 
            int 21h ; вызов прерывания для выхода из программы

        stn_label_end:
            mov [si], al
        
        loop stn_label

    pop dx cx bx ax
    ret 
string_to_num endp

; переводит массив цифр в строку  
; работает с аргументом из регистра si
num_to_string proc
    push ax bx cx dx

    xor ch, ch
    xor ah,ah

    inc si
    mov cl, [si]

    nts_label:
        inc si

        mov al, [si]
        
        cmp al, '-'
        je nts_label_end

        cmp al, 10
        jge nts_uAZ

        add al, 48
        jmp nts_label_end

        nts_uAZ:
            add al, 55
           
        nts_label_end:
            mov [si], al
        loop nts_label

    pop dx cx bx ax
    ret 
num_to_string endp

do_operations proc
    ;ввод строк
	mov ah, 09h
    lea dx, rules
    int 21h

    mov ah, 09h
    lea dx, input1
    int 21h
	
    mov ah, 0ah
    lea dx, num1
    int 21h

    mov ah, 09h
    lea dx, input2
    int 21h

    mov ah, 0ah
    lea dx, num2
    int 21h

    ; преобразуем строки к числам 
    lea si, num1
    call string_to_num
    lea si, num2
    call string_to_num
    call print_newline

    ; сложение
    mov ah, 09h
    lea dx, plus_str
    int 21h

    call plus_operate

    lea si, res
    call num_to_string

    mov ah, 09h
    lea dx, res[2]
    int 21h

	mov from_plus[0], 0 ; сбросим значения прихода от сложения

    ; вычитание
    mov ah, 09h
    lea dx, minus_str
    int 21h

    call minus_foo

    lea si, res
    call num_to_string
    mov ah, 09h
    lea dx, res[2]
    int 21h

    ; произведение
    mov ah, 09h
    lea dx, mult_str
    int 21h

    call mul_foo

    lea si, res
    call num_to_string
    mov ah, 09h
    lea dx, res[2]
    int 21h

    ret
do_operations endp

start:	
;начальные настройки
	mov ax, data
	mov ds, ax
	mov es, ax
	xor ax, ax

    mov ah, 09h
    lea dx, mode10_str
    int 21h
    call do_operations

    mov divider[0], 16
	; сбрасываем начальную позицию первого элемента
	mov first_pos_num1[0], 1
	mov first_pos_num2[0], 1

    mov ah, 09h
    lea dx, mode16_str
    int 21h
    call do_operations

    mov ah, 4ch
    int 21h
    code ends
end start