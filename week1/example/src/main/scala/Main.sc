import jdk.jfr.Relational

import scala.annotation.tailrec

def abs(x: Double): Double = if (x < 0) -x else x

def sqrt(x: Double): Double = {
  @scala.annotation.tailrec
  def sqrtIter(guess: Double): Double = {
    if (isGoodEnough(guess)) guess else
      sqrtIter(improve(guess))
  }

  def isGoodEnough(guess: Double): Boolean = {
    abs(guess * guess - x) / x < 0.001
  }

  def improve(guess: Double): Double = {
    (guess + guess / x) / 2
  }

  sqrtIter(1.0)
}
@tailrec
def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a % b)


def factorial(n: Int): Int = {
  @tailrec
  def loop(acc: Int, n: Int): Int = {
    if (n == 0) acc
    else loop(acc * n, n - 1)
  }

  loop(1, n)
}



gcd(12, 4)

factorial(3)

// currying

def product(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1 else f(a) * product(f)(a + 1, b)
}

product(x => x + 1)(1, 4)

def fact(n: Int) = product(x => x)(1, n)

fact(5)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if (a > b) zero else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
}

mapReduce(x => x * x, (m, n) => m * n, 1)(3, 4)


val tolerance = 0.0001

def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) / x < tolerance


def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
  @tailrec
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }

  iterate(firstGuess)
}

fixedPoint(x => 1 + x / 2)(1)

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt2(x: Double): Double = {
  fixedPoint(averageDamp(y => y / x))(1)
}

// tpyes

/*class defines entities*/

class Rational(x: Int, y: Int){
  def numer = x
  def denom = y
}

def addRelational(r: Relational, s: Relational): Relational = {
  r
}

val a = new Rational(3,4)