package org.appliedtopology.tda4s

import language.experimental.modularity
import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckPropertyChecks}
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.shouldEqual

import scala.Console.in
import scala.math.{cos, sin}
import scala.collection.Set

class PersistentHomologySpec extends AnyFlatSpec:
  "The tetrahedron" should "have barcode" in {
    val simplexstream = Seq(
      Set(0),
      Set(1),
      Set(2),
      Set(3),
      Set(0, 1),
      Set(0, 2),
      Set(0, 3),
      Set(1, 2),
      Set(1, 3),
      Set(2, 3),
      Set(0, 1, 2),
      Set(0, 1, 3),
      Set(0, 2, 3),
      Set(1, 2, 3)
    ).iterator
    val filtrationValues: PartialFunction[Set[Int], Double] = Map(
      Set(0) -> 0.0,
      Set(1) -> 1.0,
      Set(2) -> 2.0,
      Set(3) -> 3.0,
      Set(0, 1) -> 4.0,
      Set(0, 2) -> 5.0,
      Set(0, 3) -> 6.0,
      Set(1, 2) -> 7.0,
      Set(1, 3) -> 8.0,
      Set(2, 3) -> 9.0,
      Set(0, 1, 2) -> 10.0,
      Set(0, 1, 3) -> 11.0,
      Set(0, 2, 3) -> 12.0,
      Set(1, 2, 3) -> 13.0
    )
    val expectedBarcode = Seq(
      (0, 0.0, Double.PositiveInfinity),
      (0, 1.0, 4.0),
      (0, 2.0, 5.0),
      (0, 3.0, 6.0),
      (1, 7.0, 10.0),
      (1, 8.0, 11.0),
      (1, 9.0, 12.0),
      (2, 13.0, Double.PositiveInfinity)
    )
    given DoubleIsField(1e-15)
    PersistentHomology.persistentHomology(simplexstream, filtrationValues).sorted shouldEqual expectedBarcode.sorted
  }

class VietorisRipsValidation extends AnyFlatSpec with Checkers with Matchers:
  "the circle" should "have barcode" in {
    given FiniteFieldIsField(17)
    val D = 3
    val N = 10
    val aa = (0 until N).map(_ / N.toDouble)
    val xy = aa.map(a => Array(cos(2 * 3.14 * a), sin(2 * 3.14 * a))).toSeq
    val metricSpace = VectorMetricSpace("euclidean", xy)
    val vrStream = NaiveVietorisRips(metricSpace)
    val barcode = PersistentHomology.persistentHomology(vrStream.simplices().filter(_.size <= D + 1), vrStream.filtrationValues)
    val salientBars = barcode.filter((bar: (Int, Double, Double)) => bar._1 < D)
    val numBars = salientBars
      .map((bar: (Int, Double, Double)) => bar._3 - bar._2)
      .filter(_ > 1.0)
      .size
    numBars shouldEqual 2
  }

class KleinBottleValidation extends AnyFlatSpec with Checkers with Matchers:
  "the Klein bottle" should "have different barcodes" in {
    val topSimplices = Seq(
      Set(2,6,7), Set(0,6,7), Set(2,5,7), Set(0,5,7), Set(4,5,6), Set(3,5,6),
      Set(0,4,6), Set(2,3,6), Set(1,4,5), Set(0,3,5), Set(1,2,5), Set(2,3,4),
      Set(1,3,4), Set(0,2,4), Set(0,1,3), Set(0,1,2))
    val simplices = topSimplices
      .toSet
      .flatMap((s:Set[Int]) => s.subsets())
      .filter(_.size > 0)
      .toSeq.sorted.sortBy(_.size)
    println(simplices)
    val filtrationValues : PartialFunction[Set[Int], Double] = { case _ => 0.0 }
    val barcode0 = {
      given DoubleIsField(1e-15)
      PersistentHomology.persistentHomology(simplices.iterator, filtrationValues)
    }
    val barcode2 = {
      given FiniteFieldIsField(2)
      PersistentHomology.persistentHomology(simplices.iterator, filtrationValues)
    }
    val barcode3 = {
      given FiniteFieldIsField(3)
      PersistentHomology.persistentHomology(simplices.iterator, filtrationValues)
    }
    println(barcode0)
    println(barcode2)
    println(barcode3)
    assert(barcode0 == barcode3)
    assert(barcode0 != barcode2)
  }
