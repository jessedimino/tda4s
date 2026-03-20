package org.appliedtopology.tda4s

import language.experimental.modularity





trait Field:
/** 
  * The Field trait provides the backbone for the mathematical field structure. Fields contain two binary operations, addition and multiplication, identities for the corresponding operations, and 
  * inverses for every element except the additive identity. The field trait ensures that we have access to these operations, and we provide a means to overload the usual symbols to return addition and 
  * multiplication with respect to the field structure
  */
  type Self
  def apply(x: Int): Self
  def zero: Self
  def one: Self
  def add(x: Self, y: Self): Self
  def sub(x: Self, y: Self): Self
  def mul(x: Self, y: Self): Self
  def div(x: Self, y: Self): Self

  extension (x: Self)
    def +(y: Self): Self = add(x, y)
    def -(y: Self): Self = sub(x, y)
    def *(y: Self): Self = mul(x, y)
    def /(y: Self): Self = div(x, y)

object Field:
  /** Convenience method to create a field instance based on the given field characteristic. If the parameter is 0, the field instance
    * corresponds to `Double` as a field. If the parameter is a prime number, the field instance corresponds to a finite field of order `p`.
    *
    * @param p
    *   The field characteristic. If `p` is 0, the method returns an instance of `Double` as a field. If `p` is a prime number, the method
    *   returns an instance of `FiniteField(p)`.
    */
  def apply(p: Int) = p match {
    case 0                                  => summon[Double is Field]
    case p if BigInt(p).isProbablePrime(10) => FiniteField(p).given_ffp_is_field
  }

  given (Double is Field) = new Field:
    type Self = Double

    override def apply(x: Int): Double = x.toDouble

    def zero: Self = 0.0
    def one: Self = 1.0
    def add(x: Self, y: Self): Self = x + y
    def sub(x: Self, y: Self): Self = x - y
    def mul(x: Self, y: Self): Self = x * y
    def div(x: Self, y: Self): Self = x / y



  case class FiniteField(p: Int) extends Field:
    /**
    * We implement the finite field as a case class. In order to satisfy the field structure the field characteristic $p$ must be a prime number to ensure that multiplicative inverses exist for all elements
    * except the additive identity. 
    * @param p
    *   The field characteristic, all operations in the field are performed modulo p 
    */
    require(p > 1 && BigInt(p).isProbablePrime(10), "p must be a prime number.")
    private val inverseTable: Array[Int] = Array.tabulate(p) { i =>
      if (i == 0) 0
      else BigInt(i).modInverse(p).toInt
    }
    opaque type Self = Int
    override def apply(x: Int): Self = (x % p + p) % p
    def unapply(x: Self): Option[Int] = Some(x)
    override def toString: String = s"FiniteField($p)"
    given given_ffp_is_field: (Self is Field) = this

    def zero: Self = 0
    def one: Self = 1
    def add(x: Self, y: Self): Self = (x + y) % p
    def sub(x: Self, y: Self): Self = (x - y + p) % p
    def mul(x: Self, y: Self): Self = (x * y) % p

    def div(x: Self, y: Self): Self =
      require(y != 0, "Division by zero in modular arithmetic is undefined.")
      mul(x, inverseTable(y))

    extension (x: Self)
      def toString: String = s"FF($x)"
      override def +(y: Self): Self = add(x, y)
      override def -(y: Self): Self = sub(x, y)
      override def *(y: Self): Self = mul(x, y)
      override def /(y: Self): Self = div(x, y)
