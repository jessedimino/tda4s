package org.appliedtopology.tda4s

trait Field[T]:
  opaque type F = T
  object F:
    def apply(t: T): F = t
    def unapply(t: F): Option[T] = Some(t)
    def fromInt(n: Int): F = repAdd(one, n)

  def add(x: F, y: F): F
  def sub(x: F, y: F): F
  def mul(x: F, y: F): F
  def div(x: F, y: F): F
  def neg(x: F): F
  def inv(x: F): F
  def pow(x: F, n: Int): F
  def repAdd(x: F, n: Int): F
  def zero: F
  def one: F
  def equiv(x: F, y: F): Boolean = x == y
  def isZero(x: F): Boolean = equiv(x, zero)
  extension (x: F)
    def +(y: F): F = add(x, y)
    def -(y: F): F = sub(x, y)
    def *(y: F): F = mul(x, y)
    def /(y: F): F = div(x, y)
    def unary_- : F = neg(x)
    def **(n: Int): F = pow(x, n)
    def *(n: Int): F = repAdd(x, n)
    def toString: String = s"F($x)"

class DoubleIsField(val eps: Double) extends Field[Double]:
  def add(x: F, y: F): F = (x, y) match { case (F(xx), F(yy)) => F(xx + yy) }
  def sub(x: F, y: F): F = (x, y) match { case (F(xx), F(yy)) => F(xx - yy) }
  def mul(x: F, y: F): F = (x, y) match { case (F(xx), F(yy)) => F(xx * yy) }
  def div(x: F, y: F): F = (x, y) match { case (F(xx), F(yy)) => F(xx / yy) }
  def neg(x: F): F = x match { case F(xx) => F(-xx) }
  def inv(x: F): F = one / x
  def pow(x: F, n: Int): F = x match { case F(f) => F(math.pow(f, n)) }
  def repAdd(x: F, n: Int): F = x match { case F(xx) => F(xx * n) }
  def zero: F = F(0.0)
  def one: F = F(1.0)
  override def equiv(x: F, y: F): Boolean = (x, y) match { case (F(xx), F(yy)) => math.abs(xx - yy) < eps }

class FiniteFieldIsField(val p: Int) extends Field[Int]:
  require(p > 1 && BigInt(p).isProbablePrime(10), "p must be a prime number.")
  private val inverseTable: Array[Int] = Array.tabulate(p) { i =>
    if (i == 0) 0
    else BigInt(i).modInverse(p).toInt
  }
  def add(x: F, y: F): F = (x, y) match { case (F(xx), F(yy)) => F((xx + yy) % p) }
  def sub(x: F, y: F): F = add(x, neg(y))
  def mul(x: F, y: F): F = (x, y) match { case (F(xx), F(yy)) => F((xx * yy) % p) }
  def div(x: F, y: F): F = mul(x, inv(y))
  def neg(x: F): F = x match { case F(xx) => F(p - xx) }
  def inv(x: F): F = x match { case F(xx) => F(inverseTable(xx)) }
  def pow(x: F, n: Int): F = x match { case F(xx) => F(math.pow(xx.toDouble, n).toInt % p) }
  def repAdd(x: F, n: Int): F = x match { case F(xx) => F((xx * n) % p) }
  def zero: F = F(0)
  def one: F = F(1)
