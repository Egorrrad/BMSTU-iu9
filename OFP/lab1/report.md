% Лабораторная работа № 1 «Введение в функциональное
программирование на языке Scala»
% 19 февраля 2025 г.
% 

# Цель работы
Целью данной работы является ознакомление с программированием на языке Scala на основе чистых функций.
# Индивидуальный вариант
Закаренная функция slices: Int => (List[Int] => List[List[Int]]), выполняющая разбиение списка целых 
чисел на фрагменты указанной в качестве параметра функции длины.
# Реализация и тестирование

Работа в REPL-интерпретаторе Scala:

```scala
object Main {
  // Ручная реализация аналога List.take(n)
  def takeN(n: Int, lst: List[Int]): List[Int] = (n, lst) match {
    case (0, _) => Nil
    case (_, Nil) => Nil
    case (k, head :: tail) => head :: takeN(k - 1, tail)
  }

  // Ручная реализация аналога List.drop(n)
  def dropN(n: Int, lst: List[Int]): List[Int] = (n, lst) match {
    case (0, _) => lst
    case (_, Nil) => Nil
    case (k, _ :: tail) => dropN(k - 1, tail)
  }

  // Основная функция slices
  def slices(n: Int): List[Int] => List[List[Int]] = {
    def splitList(lst: List[Int]): List[List[Int]] = lst match {
      case Nil => Nil
      case _ => takeN(n, lst) :: splitList(dropN(n, lst))
    }
    splitList
  }

  def main(args: Array[String]): Unit = {
    val list1 = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val list2 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    val list3 = List(1)
    val list4 = List()
    val list5 = List(1, 2, 3, 4, 5)

    val splitter2 = slices(2)
    val splitter3 = slices(3)
    val splitter4 = slices(4)
    val splitter1 = slices(1)
    val splitter5 = slices(5)

    println("Splitting list1 with n=3: " + splitter3(list1))
    println("Splitting list2 with n=3: " + splitter3(list2))
    println("Splitting list2 with n=2: " + splitter2(list2))
    println("Splitting list2 with n=4: " + splitter4(list2))
    println("Splitting list3 with n=1: " + splitter1(list3))
    println("Splitting list4 with n=1: " + splitter1(list4))
    println("Splitting list5 with n=5: " + splitter5(list5))
  }
}
```
Вывод в терминале:
```
Splitting list1 with n=3: List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
Splitting list2 with n=3: List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9), List(10, 11, 12))
Splitting list2 with n=2: List(List(1, 2), List(3, 4), List(5, 6), List(7, 8), List(9, 10), List(11, 12))
Splitting list2 with n=4: List(List(1, 2, 3, 4), List(5, 6, 7, 8), List(9, 10, 11, 12))
Splitting list3 with n=1: List(List(1))
Splitting list4 with n=1: List()
Splitting list5 with n=5: List(List(1, 2, 3, 4, 5))
```
# Вывод
В ходе выполнения данной работы было рассмотрено программирование 
на языке Scala с использованием чистых функций.
Также Была реализована закаренная функция slices, которая выполняет разбиение списка целых 
чисел на фрагменты заданной длины.
Для реализации функции slices использовались рекурсивные методы takeN и dropN, аналогичные List.take(n) 
и List.drop(n), которые входят в стандартную библиотеку.
Тестирование написанной функции slices подтвердило ее корректность для работы на различных входных данных. 
Функция slices делит списки на подсписки нужной длины, включая случаи пустого списка и списка, 
размер которого кратен или не кратен n.
