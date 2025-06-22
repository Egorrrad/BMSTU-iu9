% Лабораторная работа № 4 «Case-классы и сопоставление с образцом в Scala»
% 16 апреля 2025 г.
% 

# Цель работы
Целью данной работы является приобретение навыков разработки case-классов 
на языке Scala для представления абстрактных синтаксических деревьев.

# Индивидуальный вариант
Абстрактный синтаксис выражений с let:

Expr → Expr + Expr | Expr * Expr | VARNAME | let VARNAME = Expr in Expr

Требуется написать функцию letsOptimize : Expr => Expr, которая выполняет оптимизацию выражения:

1. Если в выражении есть общие подвыражения, то общий код выносится в let:
   (x + y) * (x + y) → let v0 = x + y in v0 * v0

2. Если переменная, определённая let'ом, в выражении встречается 
только один раз, следует удалить let и подставить 
вместо переменной выражение:
   let x = y + z in a * x → a * (y + z)

Указания:
- В выражениях имена переменных в операциях let не могут повторяться
  (например, (let x = y + z in x + y) * (let x = z * z in y * z) — ошибочное выражение)
- После let нельзя использовать свободную переменную выражения
  (например, x + (let x = y * y in x * y) — ошибочное выражение)
  Эти ограничения упростят преобразование выражений.

# Реализация

```scala
package lab4

sealed trait AstNode

case class Combine(left: AstNode, right: AstNode) extends AstNode
case class Product(left: AstNode, right: AstNode) extends AstNode
case class Symbol(identifier: String) extends AstNode
case class Binding(identifier: String, boundValue: AstNode, inner: AstNode) extends AstNode

object Main {
  private var generation = 0
  private def generateLabel(): String = {
    generation += 1
    s"v$generation"
  }

  def analyzeFrequency(expression: AstNode): Map[AstNode, Int] = {
    def traverse(node: AstNode, frequencies: Map[AstNode, Int]): Map[AstNode, Int] = {
      val updatedFreq = frequencies.updatedWith(node)(_.map(_ + 1).orElse(Some(1)))
      node match {
        case Combine(l, r) => traverse(r, traverse(l, updatedFreq))
        case Product(l, r) => traverse(r, traverse(l, updatedFreq))
        case Binding(_, v, b) => traverse(b, traverse(v, updatedFreq))
        case _ => updatedFreq
      }
    }
    traverse(expression, Map.empty)
  }

  def replaceSymbol(node: AstNode, target: String, replacement: AstNode): AstNode = node match {
    case Symbol(id) if id == target => replacement
    case Combine(l, r) => Combine(replaceSymbol(l, target, replacement), 
      replaceSymbol(r, target, replacement))
    case Product(l, r) => Product(replaceSymbol(l, target, replacement), 
      replaceSymbol(r, target, replacement))
    case Binding(id, v, b) if id != target =>
      Binding(id, replaceSymbol(v, target, replacement), replaceSymbol(b, target, replacement))
    case other => other
  }

  def simplifyBindings(node: AstNode): AstNode = node match {
    case Binding(id, value, body) =>
      val usageCount = calculateUsage(body, id)
      if (usageCount <= 1) simplifyBindings(replaceSymbol(body, id, value))
      else Binding(id, simplifyBindings(value), simplifyBindings(body))
    case Combine(l, r) => Combine(simplifyBindings(l), simplifyBindings(r))
    case Product(l, r) => Product(simplifyBindings(l), simplifyBindings(r))
    case _ => node
  }

  private def calculateUsage(node: AstNode, target: String): Int = node match {
    case Symbol(id) if id == target => 1
    case Combine(l, r) => calculateUsage(l, target) + calculateUsage(r, target)
    case Product(l, r) => calculateUsage(l, target) + calculateUsage(r, target)
    case Binding(id, v, b) if id != target =>
      calculateUsage(v, target) + calculateUsage(b, target)
    case _ => 0
  }

  def letsOptimize(root: AstNode): AstNode = {
    val frequencyMap = analyzeFrequency(root)
    val commonSubexpressions = frequencyMap.filter(_._2 > 1).keys.toSeq

    val (optimized, _) = commonSubexpressions.foldLeft((root, Map.empty[AstNode, String])) {
      case ((currentExpr, env), subExpr) =>
        val freshName = generateLabel()
        val updatedExpr = substituteExpression(currentExpr, subExpr, Symbol(freshName))
        (Binding(freshName, subExpr, updatedExpr), env + (subExpr -> freshName))
    }
    simplifyBindings(optimized)
  }

  private def substituteExpression(original: AstNode, target: AstNode, replacement: AstNode): AstNode = {
    if (original == target) replacement else original match {
      case Combine(l, r) => Combine(substituteExpression(l, target, replacement), 
        substituteExpression(r, target, replacement))
      case Product(l, r) => Product(substituteExpression(l, target, replacement), 
        substituteExpression(r, target, replacement))
      case Binding(id, v, b) => Binding(id, substituteExpression(v, target, replacement), 
        substituteExpression(b, target, replacement))
      case _ => original
    }
  }

  def serializeExpression(node: AstNode): String = node match {
    case Symbol(id) => id
    case Combine(l, r) => s"(${serializeExpression(l)} + ${serializeExpression(r)})"
    case Product(l, r) => s"(${serializeExpression(l)} * ${serializeExpression(r)})"
    case Binding(id, v, b) => s"let $id = ${serializeExpression(v)} in ${serializeExpression(b)}"
  }

  def main(args: Array[String]): Unit = {
    def test(expr: AstNode): Unit = {
      println("Исходное выражение:")
      println(serializeExpression(expr))
      val optimized = letsOptimize(expr)
      println("Оптимизированное выражение:")
      println(serializeExpression(optimized))
      println()
    }

    val test1 = Product(
      Combine(Symbol("x"), Symbol("y")),
      Combine(Symbol("x"), Symbol("y"))
    )

    val test2 = Binding("x",
      Combine(Symbol("y"), Symbol("z")),
      Product(Symbol("a"), Symbol("x"))
    )

    val test3 = Product(
      Combine(Symbol("a"), Symbol("b")),
      Combine(
        Combine(Symbol("a"), Symbol("b")),
        Symbol("c"))
    )

    val test4 = Combine(
      Combine(Symbol("a"), Symbol("b")),
      Combine(Symbol("c"), Symbol("d"))
    )

    Seq(test1, test2, test3, test4).foreach(test)
  }
}
```

# Тестирование

```scala
Исходное выражение:
  ((x + y) * (x + y))
Оптимизированное выражение:
  let v1 = (x + y) in (v1 * v1)

Исходное выражение:
  let x = (y + z) in (a * x)
Оптимизированное выражение:
  (a * (y + z))

Исходное выражение:
  ((a + b) * ((a + b) + c))
Оптимизированное выражение:
  let v4 = (a + b) in (v4 * (v4 + c))

Исходное выражение:
  ((a + b) + (c + d))
Оптимизированное выражение:
  ((a + b) + (c + d))
```

# Вывод
В ходе выполнения лабораторной работы была разработана программа на языке Scala для оптимизации выражений 
с использованием case-классов. Реализована иерархия классов для представления 
абстрактного синтаксического дерева, включающая операции сложения, умножения, переменные и let-выражения. 
Основной функционал программы - функция letsOptimize - 
успешно выполняет две ключевые оптимизации: вынос общих 
подвыражений в let-блоки и удаление избыточных let с однократным использованием переменных. 
Тестирование на различных примерах выражений подтвердило корректность работы программы - все оптимизации 
выполняются в соответствии с требованиями задания.