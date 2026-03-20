package org.appliedtopology.tda4s

import Ordering.Implicits.*
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap


case class Chain(entries: SortedMap[Set[Int], Double]):
/**
* Chains are objects that carry information about how simplexes are connected. They are linear combinations of simplexes with coefficients drawn over a field. We implement them are a sorted map from simplexes
* to the corresponding coefficient. Sorting should occur by filtration value in the simplex stream.
*/
  def scaleAdd(scale: Double, other: Chain): Chain =
  /**
  * Implementation for some chain-based operation. I need to spend a bit more time studying this to better understand what it's doing. Possibly the syntax could be improved.
  */
    Chain(other.entries.foldLeft(entries) { case (result, (s, v)) =>
      entries.getOrElse(s, 0.0) match {
        case v0: Double =>
          v0 + scale * v match {
            case v1 if math.abs(v1) < 1e-10 => result.removed(s)
            case v1                         => result.updated(s, v1)
          }
      }
    })
def isZero: Boolean = entries.values.forall(math.abs(_) < 1e-10)

//chain object
object Chain:
  /**
  * Chains are equipped with boundary operators which express a k-simplex as a k-1 dimensional chain, representing the faces of the simplex.
  * These can be linked together to form a chain complex
  */

  def boundary(simplex: Set[Int])(ordering: Ordering[Set[Int]]): Chain =
    Chain(
      SortedMap.from(
        simplex.toList.sorted
          .map(i => simplex - i)
          .zip(Iterator.iterate(1.0)(-_))
      )(using ordering)
    )

  @tailrec
  def reduceBy(chain: Chain, basis: Map[Set[Int], Chain], log: Map[Set[Int], Double] = Map.empty): (Chain, Map[Set[Int], Double]) =
  /**
  * Implementation for reducing chains by a basis using tail recursion to improve efficiency. I'm still a bit shaky on the details of this
  */
    (for {
      (leadingSimplex, leadingValue) <- chain.entries.headOption
      basisChain <- basis.get(leadingSimplex)
      (basisSimplex, basisValue) <- basisChain.entries.headOption
    } yield (basisSimplex, -leadingValue / basisValue, chain.scaleAdd(-leadingValue / basisValue, basisChain))) match {
      case Some((basisSimplex, coeff, reducedChain)): Option[(Set[Int], Double, Chain)] =>
        reduceBy(reducedChain, basis, log.updated(basisSimplex, coeff))
      case None: Option[Chain] => (chain, log)
    }
