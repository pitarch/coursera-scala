package calculator

import scala.util.DynamicVariable

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {

    try {
      namedExpressions.mapValues {
        s => Signal[Double] {
          eval(s(), namedExpressions)
        }
      }.toMap
    }
    finally {

    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) => evalRef(name, references)
      case Plus(a, b) =>
        val ares = eval(a, references)
        val bres = eval(b, references)
        if (ares != Double.NaN && bres != Double.NaN) ares + bres else Double.NaN

      case Minus(a, b) =>
        val ares = eval(a, references)
        val bres = eval(b, references)
        if (ares != Double.NaN && bres != Double.NaN) ares - bres else Double.NaN

      case Times(a, b) =>
        val ares = eval(a, references)
        val bres = eval(b, references)
        if (ares != Double.NaN && bres != Double.NaN) ares * bres else Double.NaN
      case Divide(a, b) =>
        val ares: Double = eval(a, references)
        val bres: Double = eval(b, references)
        if (ares != Double.NaN && bres != Double.NaN) ares / bres
        else Double.NaN
    }
  }

  private val dynVisitedRef = new DynamicVariable[Set[String]](Set.empty)

  private def evalRef(name: String, references: Map[String, Signal[Expr]]): Double = {

    if (dynVisitedRef.value.contains(name)) {
      Double.NaN
    } else {
      dynVisitedRef.withValue(dynVisitedRef.value + name) {
        eval(this.getReferenceExpr(name, references), references)
      }
    }
  }

  /** Get the Expr for a referenced variables.
   * If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
