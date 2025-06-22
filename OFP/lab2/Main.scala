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
