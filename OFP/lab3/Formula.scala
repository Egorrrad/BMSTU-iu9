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