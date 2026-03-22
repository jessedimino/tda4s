package org.appliedtopology.tda4s

import language.experimental.modularity
import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.matchers.should.Matchers
import sun.jvm.hotspot.oops.DoubleField

class DoublePropertiesValidation extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  val doubleIsField = DoubleIsField(1e-15)
  given Field[Double] = doubleIsField
  import doubleIsField._
  def field(n: Int): F = F.fromInt(n)

  property("additive identity") {
    forAll { (x: Int) =>
      equiv(field(x) + zero, field(x)) should be
    }
  }

  property("additive commutativity") {
    forAll { (x: Int, y: Int) =>
      equiv(field(x) + field(y), field(y) + field(x)) should be
    }
  }

  property("multiplicative identity") {
    forAll { (x: Int) =>
      equiv(field(x) * one, field(x)) should be
    }
  }

  property("multiplicative commutativity") {
    forAll { (x: Int, y: Int) =>
      equiv(field(x) * field(y), field(y) * field(x)) should be
    }
  }

  property("distributivity") { // runs into floating point equality issues
    forAll { (x: Int, y: Int, z: Int) =>
      equiv(field(x) * (field(y) + field(z)), field(x) * field(y) + field(x) * field(z)) should be
    }
  }

  property("division reversal") {
    forAll { (x: Int, y: Int) =>
      (y != 0) ==>
        equiv((field(x) / field(y)) * field(y), field(x))
    }
  }

class FF17PropertiesValidation extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  val ff17IsField = FiniteFieldIsField(17)
  given Field[Int] = ff17IsField
  import ff17IsField._
  def field(n: Int): F = F.fromInt(n)

  property("additive identity") {
    forAll { (x: Int) =>
      equiv(field(x) + zero, field(x)) should be
    }
  }

  property("additive commutativity") {
    forAll { (x: Int, y: Int) =>
      equiv(field(x) + field(y), field(y) + field(x)) should be
    }
  }

  property("multiplicative identity") {
    forAll { (x: Int) =>
      equiv(field(x) * one, field(x)) should be
    }
  }

  property("multiplicative commutativity") {
    forAll { (x: Int, y: Int) =>
      equiv(field(x) * field(y), field(y) * field(x)) should be
    }
  }

  property("distributivity") { // runs into floating point equality issues
    forAll { (x: Int, y: Int, z: Int) =>
      equiv(field(x) * (field(y) + field(z)), field(x) * field(y) + field(x) * field(z)) should be
    }
  }

  property("division reversal") {
    forAll { (x: Int, y: Int) =>
      (y != 0) ==>
        equiv((field(x) / field(y)) * field(y), field(x))
    }
  }
