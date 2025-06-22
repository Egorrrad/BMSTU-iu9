% Лабораторная работа № 1 «Введение в объетно-ориентированное
программирование на языке Scala»
% 5 марта 2025 г.
% 

# Цель работы
Целью данной работы является изучение базовых объектно-ориентированных возможностей языка Scala.

# Индивидуальный вариант
80-битовое целое число со знаком, представленное списом из пяти Short’ов, 
с операциями сложения, вычитания, умножения и изменения знака (унарный минус).

# Реализация и тестирование

Файл `Int80.scala`
```scala
package lab2

class Int80 private (private val chunks: List[Short]) {
  // число представляется как список из 5 значений типа Short (16 бит каждое)
  require(chunks.size == 5, "80-битное число должно быть представлено ровно 5 значениями Short")


  private val toBigInt: BigInt = {
    // собираем число из 5 блоков по 16 бит.
    val unsigned = chunks.foldLeft(BigInt(0)) {
      (acc, chunk) => (acc << 16) | (chunk & 0xffff) // Маска 0xffff для корректной обработки знака
    }

    // Если установлен 79-й бит, интерпретируем число как отрицательное
    if ((unsigned & (BigInt(1) << 79)) != 0) unsigned - (BigInt(1) << 80)
    else unsigned
  }

  // Арифметические операции
  def +(that: Int80): Int80 = Int80(this.toBigInt + that.toBigInt)
  def -(that: Int80): Int80 = Int80(this.toBigInt - that.toBigInt)
  def *(that: Int80): Int80 = Int80(this.toBigInt * that.toBigInt)
  def unary_- : Int80 = Int80(-this.toBigInt) // Унарный минус

  override def equals(obj: Any): Boolean = obj match {
    case that: Int80 => this.toBigInt == that.toBigInt
    case _ => false
  }

  override def toString: String = toBigInt.toString
}

// объект-компаньон для создания экземпляров Int80
object Int80 {
  val MinValue: BigInt = -(BigInt(1) << 79) // минимальное значение -2^79
  val MaxValue: BigInt = (BigInt(1) << 79) - 1 // максимальное значение 2^79 - 1
  private val Mask80bit: BigInt = (BigInt(1) << 80) - 1 // маска для 80-битного представления


  def apply(number: BigInt): Int80 = {
    val clipped = number match {
      case n if n < MinValue => MinValue // Ограничиваем снизу
      case n if n > MaxValue => MaxValue // Ограничиваем сверху
      case n => n
    }

    val masked = clipped & Mask80Bit // Убираем лишние биты

    // Разбиение числа на 5 16-битных частей (от старших битов к младшим)
    val parts = (0 until 5).map { i =>
      ((masked >> (16 * (4 - i))) & 0xffff).toShort
    }.toList

    new Int80(parts)
  }

  // Конструкторы для удобства работы с Long и Int
  def apply(long: Long): Int80 = apply(BigInt(long))
  def apply(int: Int): Int80 = apply(BigInt(int))
}
```

Файл `Main.scala`
```scala
package lab2

object Main {
  def main(args: Array[String]): Unit = {
    val num1 = Int80(100500)
    val num2 = Int80(-123456)

    val maxVal = Int80(Int80.MaxValue)
    val minVal = Int80(Int80.MinValue)

    // Проверка арифметических операций
    println(s"Сложение: 100500 + (-123456) = ${num1 + num2}")
    println(s"Разность: 100500 - 123456 = ${num1 - num2}")
    println(s"Умножение: 100500 * 2 = ${num1 * Int80(2)}")

    // Проверка граничных значений
    println(s"Максимальное 80-битное: $maxVal")
    println(s"Минимальное 80-битное: $minVal")

    // Проверка переполнения
    println(s"Переполнение (MAX + 1): ${maxVal + Int80(1)}")

    // Проверка смены знака
    println(s"Смена знака для 100500: ${-num1}")
    println(s"Смена знака для MIN: ${-minVal}")
  }
}
```

Вывод на `stdout`
```
Сложение: 100500 + (-123456) = -22956
Разность: 100500 - 123456 = 223956
Умножение: 100500 * 2 = 201000
Максимальное 80-битное: 604462909807314587353087
Минимальное 80-битное: -604462909807314587353088
Переполнение (MAX + 1): 604462909807314587353087
Смена знака для 100500: -100500
Смена знака для MIN: 604462909807314587353087
```

# Вывод
В ходе выполнения лабораторной работы научился применять объектно-ориентированный 
подход в Scala для создания неизменяемых классов. 
Освоил работу с типами данных (Short, BigInt), битовыми операциями и коллекциями. 
На практике изучил особенности языка Scala и его 
применение для решения задач, связанных с обработкой чисел произвольной точности.