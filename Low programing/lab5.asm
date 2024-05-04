; вариант 3
; удалить все вхождения заданного символа из текста.


assume cs: code, ds: data

init macro 
    mov ax, data
	mov ds, ax
	mov es, ax
	xor ax, ax
endm

print macro str
    push ax dx
	mov dx, offset str
	mov ah, 09h
	int 21h
	pop dx ax
endm

input macro src 
    push ax dx
	mov ah, 0ah
	lea dx, src
	int 21h
    pop dx ax
endm

if_equal macro a, b, labl
    cmp a, b 
    je labl
endm

def_copy macro src, src_ind, dst, dst_ind, jmplbl
    mov al, src[src_ind]
    mov dst[dst_ind], al
    inc src_ind
    inc dst_ind
    jmp jmplbl
endm

delete_symbol macro text, symbol
    mov si, 2
    mov di, 2

    compare:
        ; если конец строки, то заканчиваем
        if_equal text[si], 36, end_compare

        ; сравнили с символом 
        if_equal text[si], symbol, found_symbol

        ;если не наш символ, то просто копируем
        def_copy text, si, text, di, compare

        found_symbol:
            inc si
            jmp compare
     
    end_compare:
    
    mov text[di], '$'
endm


data segment

buf db 100 dup (?)
text db 100, 100, 102 dup('$')
instr1 db 'input your text: ', 10, 36
newline db 10, 36
in_put db 'input.txt', 0
out_put db 'output.txt', 0
handle dw ?
handle2 dw ?
symbol db 0
filesize dw 512

data ends

sseg segment stack
db 256 dup(0)
sseg ends

code segment

start:
	init

    open in_put, handle
    create out_put, handle2
    read handle, filesize, buf
    input buf, text, symbol

    ; print instr1
    ; input text

    delete_symbol text, 'a'

    ; print newline

    ; print text+2

	mov ah, 4ch
	int 21h
	code ends
	end start