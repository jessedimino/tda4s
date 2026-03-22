package org.appliedtopology.tda4s

import Ordering.Implicits.*
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.{Map, Set}

class Chain[T](using val field: Field[T])(val simplexCoefficients: SortedMap[Set[Int], field.F]):
  import field.*
  def scale(scalingFactor: F): Chain[T] = Chain(simplexCoefficients.collect[Set[Int], F] { case (k, v) =>
    (k, v * scalingFactor)
  }(using simplexCoefficients.ordering))
  def scaleAdd(scalingFactor: F, other: Chain[T]): Chain[T] =
    Chain(other.simplexCoefficients.foldLeft(simplexCoefficients) { case (result, (s, v)) =>
      result.getOrElse(s, zero) match {
        case v0: F =>
          v match {
            case v1: F =>
              v0 + scalingFactor * v1 match {
                case v2 if field.isZero(v2) => result.removed(s)
                case v2                     => result.updated(s, v2)
              }
          }
      }
    })
  def isZero: Boolean = simplexCoefficients.values.forall(field.isZero(_))
  def leading: Option[(Set[Int], F)] = simplexCoefficients.lastOption

object Chain:
  def from[T: Field as field](simplex: Set[Int])(ordering: Ordering[Set[Int]]): Chain[T] = Chain(
    SortedMap((simplex, field.one))(using ordering)
  )
  def boundary[T: Field as field](simplex: Set[Int])(ordering: Ordering[Set[Int]]): Chain[T] = {
    val vertices = simplex.toSeq.sorted
    val faces = vertices.map(i => simplex - i)
    val coefficients = Iterator.iterate(field.one)(field.neg)
    val faceCoefficientPairs = faces.zip(coefficients).toSeq
    val entries = SortedMap(faceCoefficientPairs*)(using ordering)
    Chain(entries)
  }

  def reduce[T: Field as outerField](chain: Chain[T], basis: Map[Set[Int], Chain[T]]): (Chain[T], Map[Set[Int], outerField.F]) =
    import outerField._
    @tailrec
    def reduceBy(
        innerChain: Chain[T],
        basis: Map[Set[Int], Chain[T]],
        reductionHistory: Map[Set[Int], F]
    )(using innerField: Field[T]): (Chain[T], Map[Set[Int], F]) = innerChain.field match {
      case outerField =>
        innerChain.simplexCoefficients.find(k => basis.contains(k._1)) match {
          case None => (innerChain, reductionHistory)
          case Some((simplex, chainCoeff: F)) =>
            basis.get(simplex) match {
              case None => (innerChain, reductionHistory)
              case Some(pivotChain) =>
                val pivotCoeff: F = pivotChain.simplexCoefficients.getOrElse(simplex, zero).asInstanceOf[F]
                val resultCoeff: F = chainCoeff / pivotCoeff
                reduceBy(
                  innerChain.scaleAdd(-resultCoeff.asInstanceOf[innerChain.field.F], pivotChain),
                  basis,
                  reductionHistory + (simplex -> resultCoeff)
                )(using outerField)
            }
        }
    }
    reduceBy(chain, basis, Map.empty)(using outerField)
