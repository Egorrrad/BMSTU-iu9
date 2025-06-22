package lab3

object Main extends App {
  // Для строк
  val strFormula = Formula.variable[String]("a") + Formula.constant(" test")
  println(strFormula.evaluate(Map("a" -> "hello")))

  // Для чисел
  val intFormula = Formula.variable[Int]("x") * (Formula.constant(2) + Formula.variable[Int]("y"))
  println(intFormula.evaluate(Map("x" -> 3, "y" -> 5)))

  val doubleFormula = Formula.variable[Double]("x") * (Formula.constant(2.0) + Formula.variable[Double]("y") + Formula.variable[Double]("y")/Formula.constant(3.1))
  println(doubleFormula.evaluate(Map("x" -> 3.0, "y" -> 5.3)))

  // Неподдерживаемая операция
  val s = Formula.variable[String]("x")
  // val invalid = s * s
}