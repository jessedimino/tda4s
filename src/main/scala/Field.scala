package org.appliedtopology.tda4s

trait Coefficient:
  type Field
  def apply(x: Int): Field
  def zero: Field
  def one: Field
  def add(x: Field, y: Field): Field
  def sub(x: Field, y: Field): Field
  def mul(x: Field, y: Field): Field
  def div(x: Field, y: Field): Field

  extension (x: Field)
    def +(y: Field): Field = add(x, y)
    def -(y: Field): Field = sub(x, y)
    def *(y: Field): Field = mul(x, y)
    def /(y: Field): Field = div(x, y)

object Field:
  /** Convenience method to create a field instance based on the given field characteristic. If the parameter is 0, the field instance
    * corresponds to `Double` as a field. If the parameter is a prime number, the field instance corresponds to a finite field of order `p`.
    *
    * @param p
    *   The field characteristic. If `p` is 0, the method returns an instance of `Double` as a field. If `p` is a prime number, the method
    *   returns an instance of `FiniteField(p)`.
    */
  def apply(p: Int): Coefficient = p match {
    case 0                                  => DoubleIsField(1e-10)
    case p if BigInt(p).isProbablePrime(10) => FiniteField(p)
  }

  def DoubleIsField(eps: Double): Coefficient = new Coefficient:
    opaque type Field = Double

    override def apply(x: Int): Double = x.toDouble

    def zero: Field = 0.0

    def one: Field = 1.0

    def add(x: Field, y: Field): Field = x + y

    def sub(x: Field, y: Field): Field = x - y

    def mul(x: Field, y: Field): Field = x * y

    def div(x: Field, y: Field): Field = x / y

  def FiniteField(p: Int): Coefficient = new Coefficient:
    opaque type Field = Int
    require(p > 1 && BigInt(p).isProbablePrime(10), "p must be a prime number.")
    private val inverseTable: Array[Int] = Array.tabulate(p) { i =>
      if (i == 0) 0
      else BigInt(i).modInverse(p).toInt
    }
    override def apply(x: Int): Field = (x % p + p) % p
    def unapply(x: Field): Option[Int] = Some(x)
    override def toString: String = s"FiniteField($p)"

    def zero: Field = 0
    def one: Field = 1
    def add(x: Field, y: Field): Field = (x + y) % p
    def sub(x: Field, y: Field): Field = (x - y + p) % p
    def mul(x: Field, y: Field): Field = (x * y) % p

    def div(x: Field, y: Field): Field =
      require(y != 0, "Division by zero in modular arithmetic is undefined.")
      mul(x, inverseTable(y))
