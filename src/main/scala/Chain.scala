package org.appliedtopology.tda4s

import Ordering.Implicits.*
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

//base implementation of chain that's a sorted map of simplexes to coefficients
case class Chain(entries: SortedMap[Set[Int], Double]):
  //implementation of scaling and addition for chains
  def scaleAdd(scale: Double, other: Chain): Chain =
    //iterate over all the (s,v) pairs from left to right
    Chain(other.entries.foldLeft(entries) { case (result, (s, v)) =>
    //return s or 0 to avoid an error
      entries.getOrElse(s, 0.0) match {
        //for the initial value v0
        case v0: Double =>
          //compute v1 as this
          v0 + scale * v match {
            //remove s if v1 is too small
            case v1 if math.abs(v1) < 1e-10 => result.removed(s)
            //update (s,v0) to (s,v1) otherwise
            case v1                         => result.updated(s, v1)
          }
      }
    })
  //whole process gets wrapped in a chain
def isZero: Boolean = entries.values.forall(math.abs(_) < 1e-10)

//chain object
object Chain:
  //chains have boundary maps
  //boundary maps represent a k-dimensional chain as a linear combination of its faces which are
  //k-1 simplexes
  def boundary(simplex: Set[Int])(ordering: Ordering[Set[Int]]): Chain =
    Chain(
      SortedMap.from(
        simplex.toList.sorted
          .map(i => simplex - i)
          .zip(Iterator.iterate(1.0)(-_))
      )(using ordering)
    )

  //use tail recursion for computational efficiency
  @tailrec
  //implementation for reducing chains by a basis
  def reduceBy(chain: Chain, basis: Map[Set[Int], Chain], log: Map[Set[Int], Double] = Map.empty): (Chain, Map[Set[Int], Double]) =
    (for {
      (leadingSimplex, leadingValue) <- chain.entries.headOption
      basisChain <- basis.get(leadingSimplex)
      (basisSimplex, basisValue) <- basisChain.entries.headOption
    } yield (basisSimplex, -leadingValue / basisValue, chain.scaleAdd(-leadingValue / basisValue, basisChain))) match {
      case Some((basisSimplex, coeff, reducedChain)): Option[(Set[Int], Double, Chain)] =>
        reduceBy(reducedChain, basis, log.updated(basisSimplex, coeff))
      case None: Option[Chain] => (chain, log)
    }
