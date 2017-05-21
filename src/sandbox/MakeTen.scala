package sandbox

import scala.util.Try
import scala.util.matching.Regex

object MakeTen {

  def canMake(a: Int, b: Int, c: Int, d: Int): Boolean = {
    val numPermutation = Seq(a, b, c, d).permutations
    val orderPermutation = getOrderPermutation
    val operatorPermutation = getOperatorPermutation
    val results = for {
      nums <- numPermutation
      orders <- orderPermutation
      ops <- operatorPermutation
    } yield eval(nums, orders, ops)
    results.flatten.contains(Rational(10, 1))
  }

  def getOrderPermutation: Seq[Seq[Int]] = {
    Seq(1, 2, 3).permutations.toSeq
  }

  def getOperatorPermutation: Seq[Seq[String]] = {
    val allOps = Seq("+", "-", "*", "/")
    for {
      op1 <- allOps
      op2 <- allOps
      op3 <- allOps
    } yield Seq(op1, op2, op3)
  }

  def eval(nums: Seq[Int], orders: Seq[Int], ops: Seq[String]): Option[Rational] = {
    solveRpn(makeRpn(nums, orders, ops))
  }

  def makeRpn(nums: Seq[Int], orders: Seq[Int], ops: Seq[String]): Seq[String] = {
    (orders zip ops).foldLeft(nums.map(_.toString)) { case (acc, (order, op)) =>
      val i = index(acc, (s: String) => s.matches("""\d"""), order)
      if (acc.drop(i).exists(isOperator))
        insert(acc, op, acc.lastIndexWhere(isOperator) + 1)
      else
        insert(acc, op, i + 1)
    }
  }

  /**
    * pを満たすa番目の要素(zero-origin)のindex
    * e.g. index(Seq(0,1,2,3,4,5,6,7,8,9), i: Int => i % 2 == 0, 2))
    *  -> 4
    */
  def index[A](xs: Seq[A], p: A => Boolean, a: Int): Int = {
    @scala.annotation.tailrec
    def f[B](xs: Seq[B], p: B => Boolean, a: Int, b: Int): Int = {
      require(a >= 0)
      val i = xs.indexWhere(p)
      if (a == 0) b + i else f(xs.drop(i + 1), p, a - 1, b + i + 1)
    }
    f(xs, p, a, 0)
  }

  def isOperator(s: String): Boolean = Seq("+", "-", "*", "/").contains(s)

  def insert[A](as: Seq[A], a: A, i: Int): Seq[A] = {
    val (before, after) = as.splitAt(i)
    before ++ (a +: after)
  }

  def solveRpn(strings: Seq[String]): Option[Rational] = {
    def foldingFunc(stack: List[String], str: String): List[String] = {
      (stack, str) match {
        case (x :: y :: ys, "+") => (Rational.parse(y) + Rational.parse(x)).toString :: ys
        case (x :: y :: ys, "-") => (Rational.parse(y) - Rational.parse(x)).toString :: ys
        case (x :: y :: ys, "*") => (Rational.parse(y) * Rational.parse(x)).toString :: ys
        case (x :: y :: ys, "/") => (Rational.parse(y) / Rational.parse(x)).toString :: ys
        case _ => str :: stack
      }
    }

    Try {
      strings.foldLeft(List.empty[String]) { case (acc, s) => foldingFunc(acc, s) }.head
    }.toOption
      .map(Rational.parse)
  }

  case class Rational(n: Int, d: Int) {
    require(d != 0)

    private def gcd(x: Int, y: Int): Int = {
      if (x == 0) y
      else if (x < 0) gcd(-x, y)
      else if (y < 0) -gcd(x, -y)
      else gcd(y % x, x)
    }

    private val g = gcd(n, d)

    val numer: Int = n / g
    val denom: Int = d / g

    def +(that: Rational) = Rational(numer * that.denom + that.numer * denom, denom * that.denom)

    def -(that: Rational) = Rational(numer * that.denom - that.numer * denom, denom * that.denom)

    def *(that: Rational) = Rational(numer * that.numer, denom * that.denom)

    def /(that: Rational) = Rational(numer * that.denom, denom * that.numer)

    override def toString: String = s"${n}/${d}"

    override def equals(other: Any): Boolean = other match {
      case that: Rational => numer == that.numer && denom == that.denom
      case _ => false
    }
  }

  object Rational {

    val rationalPattern: Regex = """(-?\d+)/(-?\d+)""".r
    val intPattern: Regex = """-?\d+""".r

    def parse(s: String): Rational = s match {
      case rationalPattern(n, d) => Rational(n.toInt, d.toInt)
      case intPattern() => Rational(s.toInt, 1)
    }

  }

}
