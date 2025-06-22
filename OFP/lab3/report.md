% Лабораторная работа № 3 «Обобщённые классы в Scala»
% 2 апреля 2025 г.
% 

# Цель работы
Целью данной работы является приобретение навыков разработки обобщённых классов на языке 
Scala с использованием неявных преобразований типов.

# Индивидуальный вариант
Класс Formula[T], представляющий формулу, состоящую из имён переменных, констант типа T и бинарных операций. 
Ассортимент операций зависит от типа T: если определён Numeric[T], то должны присутствовать четыре базовых 
арифметических операций; если T — это string, то должна присутствовать только операция сложения, 
обозначающая конкатенацию.

Для создания объектов класса Formula[T] доступны два конструктора: первый принимает имя переменной и 
создаёт формулу, состоящую из единственной переменной; второй конструктор принимает значение типа T и 
создаёт формулу, состоящую из единственной константы. Все остальные формулы возвращаются бинарными операциями.

В классе Formula[T] должен быть определён метод, принимающий отображение имён переменных в их значения и 
считающий значение формулы.

# Реализация

Файл `Formula.scala`
```scala
package lab3

class Formula[T] private (private val evalFunc: Map[String, T] => T) {
  def evaluate(vars: Map[String, T]): T = evalFunc(vars)

  def +(other: Formula[T])(implicit ops: FormulaOps[T]): Formula[T] =
    new Formula[T](vars => ops.add(this.evaluate(vars), other.evaluate(vars)))

  def -(other: Formula[T])(implicit ops: NumericFormulaOps[T]): Formula[T] =
    new Formula[T](vars => ops.subtract(this.evaluate(vars), other.evaluate(vars)))

  def *(other: Formula[T])(implicit ops: NumericFormulaOps[T]): Formula[T] =
    new Formula[T](vars => ops.multiply(this.evaluate(vars), other.evaluate(vars)))

  def /(other: Formula[T])(implicit ops: NumericFormulaOps[T]): Formula[T] =
    new Formula[T](vars => ops.divide(this.evaluate(vars), other.evaluate(vars)))
}

object Formula {
  def variable[T](name: String): Formula[T] = new Formula[T](vars => vars(name))
  def constant[T](value: T): Formula[T] = new Formula[T](_ => value)
}

trait FormulaOps[T] {
  def add(a: T, b: T): T
}

trait NumericFormulaOps[T] extends FormulaOps[T] {
  def subtract(a: T, b: T): T
  def multiply(a: T, b: T): T
  def divide(a: T, b: T): T
}

object FormulaOps {
  implicit object StringOps extends FormulaOps[String] {
    def add(a: String, b: String): String = a + b
  }

  implicit def numericOps[T](implicit num: Numeric[T]): NumericFormulaOps[T] =
    new NumericFormulaOps[T] {
      def add(a: T, b: T): T = num.plus(a, b)
      def subtract(a: T, b: T): T = num.minus(a, b)
      def multiply(a: T, b: T): T = num.times(a, b)
      def divide(a: T, b: T): T = num match {
        case frac: Fractional[T] => frac.div(a, b)
        case _ => throw new UnsupportedOperationException("Division not supported")
      }
    }
}
```

Файл `Main.scala`
```scala
package lab3

object Main extends App {
  // Для строк
  val strFormula = Formula.variable[String]("a") + Formula.constant(" test")
  println(strFormula.evaluate(Map("a" -> "hello")))

  // Для чисел
  val intFormula = Formula.variable[Int]("x") * (Formula.constant(2) + Formula.variable[Int]("y"))
  println(intFormula.evaluate(Map("x" -> 3, "y" -> 5)))

  val doubleFormula = Formula.variable[Double]("x") * 
    (Formula.constant(2.0) + Formula.variable[Double]("y") + 
    Formula.variable[Double]("y")/Formula.constant(3.1))
  println(doubleFormula.evaluate(Map("x" -> 3.0, "y" -> 5.3)))

  // Неподдерживаемая операция
  val s = Formula.variable[String]("x")
  // val invalid = s * s
}
```

# Тестирование

Результат запуска программы:

```
hello test
21
27.029032258064518

Process finished with exit code 0
```

# Вывод
В ходе лабораторной работы был разработан обобщённый класс Formula[T], позволяющий создавать формулы 
с поддержкой арифметических операций для числовых типов через Numeric[T] и конкатенации для строк. 
Реализация обеспечивает строгую проверку типов на этапе компиляции 
и предотвращает некорректные операции. Особенностью решения стало 
гибкое вычисление формул с переменными через 
передачу отображения значений в метод evaluate.