package org.appliedtopology.tda4s

import Ordering.Implicits.*
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.{Map, Set}

case class Chain(simplexCoefficients: SortedMap[Set[Int], Double]):
  def scale(scalingFactor: Double): Chain = Chain(simplexCoefficients.collect[Set[Int], Double] { case (k, v) =>
    (k, v * scalingFactor)
  }(using simplexCoefficients.ordering))
  def scaleAdd(scalingFactor: Double, other: Chain): Chain =
    Chain(other.simplexCoefficients.foldLeft(simplexCoefficients) { case (result, (s, v)) =>
      result.getOrElse(s, 0.0) match {
        case v0: Double =>
          v0 + scalingFactor * v match {
            case v1 if math.abs(v1) < 1e-10 => result.removed(s)
            case v1                         => result.updated(s, v1)
          }
      }
    })
  def isZero: Boolean = simplexCoefficients.values.forall(math.abs(_) < 1e-10)
  def leading: Option[(Set[Int], Double)] = simplexCoefficients.lastOption

object Chain:
  def from(simplex: Set[Int])(ordering: Ordering[Set[Int]]): Chain = Chain(SortedMap((simplex, 1.0))(using ordering))
  def boundary(simplex: Set[Int])(ordering: Ordering[Set[Int]]): Chain = {
    val vertices = simplex.toSeq.sorted
    val faces = vertices.map(i => simplex - i)
    val coefficients = Iterator.iterate(1.0)(-_)
    val faceCoefficientPairs = faces.zip(coefficients).toSeq
    val entries = SortedMap(faceCoefficientPairs: _*)(using ordering)
    Chain(entries)
  }

  @tailrec
  def reduceBy(
      chain: Chain,
      basis: Map[Set[Int], Chain],
      reductionHistory: Map[Set[Int], Double] = Map.empty
  ): (Chain, Map[Set[Int], Double]) =
    chain.simplexCoefficients.find(k => basis.contains(k._1)) match {
      case None => (chain, reductionHistory)
      case Some(value) =>
        val (simplex, chainCoeff) = value
        val pivotChain = basis(simplex)
        val coeff = chainCoeff / pivotChain.simplexCoefficients(simplex)
        reduceBy(chain.scaleAdd(-coeff, pivotChain), basis, reductionHistory + (simplex -> coeff))
    }
