
def abs(x: Double): Double = if (x < 0) -x else x

def sqrt(x: Double): Double = {
  def sqrtIter(guess: Double): Double = {
    if(isGoodEnough(guess)) guess else
      sqrtIter(improve(guess))
  }
  def isGoodEnough(guess: Double):Boolean = {
    abs(guess * guess - x) / x < 0.001
  }
  def improve(guess: Double):Double = {
    (guess + guess/x)/2
  }
  sqrtIter(1.0)
}

def gcd(a: Int,b: Int): Int =
  if (b==0) a else gcd(b, a % b)

def factorial(n: Int): Int = {
  def loop(acc: Int, n: Int): Int  = {
    if (n==0) acc
    else loop(acc * n, n -1)
  }
  loop(1, n)
}

gcd(12, 4)

factorial(3)
