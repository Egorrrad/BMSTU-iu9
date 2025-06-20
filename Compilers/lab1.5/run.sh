#!/bin/bash

# Шаг 1: Генерация лексического анализатора с помощью flex
flex lexer.l


if [ $? -ne 0 ]; then
    echo "Ошибка: flex не смог сгенерировать lex.yy.c"
    exit 1
fi

# Шаг 2: Компиляция сгенерированного кода
gcc lex.yy.c -o lexer -ll


if [ $? -ne 0 ]; then
    echo "Ошибка: gcc не смог скомпилировать lex.yy.c"
    exit 1
fi

# Шаг 3: Запуск программы с входным файлом input.txt
if [ -f "input.txt" ]; then
    ./lexer input.txt
else
    echo "Ошибка: файл input.txt не найден"
    exit 1
fi