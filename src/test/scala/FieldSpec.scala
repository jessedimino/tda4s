package org.appliedtopology.tda4s

import language.experimental.modularity

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, propBoolean}

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.matchers.should.Matchers

class DoublePropertiesValidation extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  val field: Coefficient = Field(0)
  import field._

  property("additive identity") {
    forAll { (x: Int) =>
      field(x) + field.zero shouldEqual field(x)
    }
  }

  property("additive commutativity") {
    forAll { (x: Int, y: Int) =>
      field(x) + field(y) shouldEqual field(y) + field(x)
    }
  }

  property("multiplicative identity") {
    forAll { (x: Int) =>
      field(x) * field.one shouldEqual field(x)
    }
  }

  property("multiplicative commutativity") {
    forAll { (x: Int, y: Int) =>
      field(x) * field(y) shouldEqual field(y) * field(x)
    }
  }

  ignore("distributivity") { // runs into floating point equality issues
    forAll { (x: Int, y: Int, z: Int) =>
      field(x) * (field(y) + field(z)) shouldEqual field(x) * field(y) + field(x) * field(z)
    }
  }

  property("division reversal") {
    forAll { (x: Int, y: Int) =>
      (y != 0) ==> {
        (field(x) / field(y)) * field(y) == field(x)
      }
    }
  }

class FF17PropertiesValidation extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  val field: Coefficient = Field(17)
  import field._

  property("additive identity") {
    forAll { (x: Int) =>
      field(x) + field.zero shouldEqual field(x)
    }
  }

  property("additive commutativity") {
    forAll { (x: Int, y: Int) =>
      field(x) + field(y) shouldEqual field(y) + field(x)
    }
  }

  property("multiplicative identity") {
    forAll { (x: Int) =>
      field(x) * field.one shouldEqual field(x)
    }
  }

  property("multiplicative commutativity") {
    forAll { (x: Int, y: Int) =>
      field(x) * field(y) shouldEqual field(y) * field(x)
    }
  }

  property("distributivity") {
    forAll { (x: Int, y: Int, z: Int) =>
      field(x) * (field(y) + field(z)) shouldEqual field(x) * field(y) + field(x) * field(z)
    }
  }

  property("division reversal") {
    forAll { (x: Int, y: Int) =>
      (y != 0) ==> {
        (field(x) / field(y)) * field(y) == field(x)
      }
    }
  }
