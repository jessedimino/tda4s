package org.appliedtopology.tda4s

import collection.mutable
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.Set

object PersistentHomology:
  def persistentHomology(
      simplexstream: Iterator[Set[Int]],
      filtrationValues: PartialFunction[Set[Int], Double]
  ): Seq[(Int, Double, Double)] =
    NaivePersistentHomology(simplexstream, filtrationValues).barcode
end PersistentHomology

class NaivePersistentHomology(simplexstream: Iterator[Set[Int]], filtrationValues: PartialFunction[Set[Int], Double]):
  def filtrationValue(simplex: Set[Int]): Double =
    filtrationValues.applyOrElse(simplex, _ => Double.PositiveInfinity)
  def streamOrdering = Ordering.by(filtrationValue).orElseBy(SortedSet.from(_))(Ordering.Implicits.sortedSetOrdering)
  case class State(
      cycleBasis: Map[Set[Int], Chain],
      boundaryBasis: Map[Set[Int], Chain],
      coboundaryLookup: Map[Set[Int], Chain],
      cycleBirths: Map[Set[Int], Double]
  )
  var state = State(Map.empty, Map.empty, Map.empty, Map.empty)
  val barcodes = mutable.ListBuffer.empty[(Int, Double, Double)]
  def step(simplex: Set[Int]): Unit = if (simplex.size <= 1) {
    val birth = filtrationValue(simplex)
    state = state.copy(
      cycleBasis = state.cycleBasis + (simplex -> Chain(
        SortedMap(simplex -> 1.0)(using streamOrdering)
      )),
      cycleBirths = state.cycleBirths + (simplex -> birth)
    )
  } else {
    val dsimplex = Chain.boundary(simplex)(streamOrdering)
    val dimension = simplex.size - 1
    assert(dsimplex.entries.size == simplex.size)
    val (dsModB, reductionB) = Chain.reduceBy(dsimplex, state.boundaryBasis)
    if (dsModB.isZero) {
      // then ∂s is already a boundary, say the boundary of z
      // hence ∂(s-z) = ∂s - ∂z = 0
      // so s-z is a new cycle
      val cycle = // look through reduction log, read off coboundaries and scale and add
        reductionB
          .foldLeft(Chain.from(simplex)(streamOrdering)) { case (chain, (spx, coeff)) =>
            chain.scaleAdd(-coeff, state.coboundaryLookup(spx))
          }

      val (cyclemodZ, _) = Chain.reduceBy(cycle, state.cycleBasis)
      val cycleBirth = filtrationValue(simplex)

      state = cyclemodZ.leading match {
        case Some((spx, _)) =>
          state.copy(
            cycleBasis = state.cycleBasis + (spx -> cyclemodZ),
            cycleBirths = state.cycleBirths + (spx -> cycleBirth)
          )
        case None: Option[(Set[Int], Double)] => state
      }
    } else {
      // since ∂s is not a boundary already, it has to be a cycle
      // reducing mod cycles will tell us which cycle just became a boundary with the addition of simplex
      // val (dsModZ, reductionZ) = Chain.reduceBy(dsModB, state.cycleBasis)
      // assert(dsModZ.isZero)
      val dyingChainHead = dsModB.entries.keys
        .filter(k => state.cycleBasis.contains(k))
        .max(using streamOrdering)
      barcodes.append((dyingChainHead.size - 1, state.cycleBirths(dyingChainHead), filtrationValue(simplex)))
      assert(simplex.size == dyingChainHead.size + 1)
      state = state.copy(
        cycleBasis = state.cycleBasis - dyingChainHead,
        boundaryBasis = state.boundaryBasis + (dsModB.leading.get._1 -> dsModB),
        cycleBirths = state.cycleBirths - dyingChainHead,
        coboundaryLookup = state.coboundaryLookup.updatedWith(dyingChainHead) {
          case Some(value) => Some(value.scaleAdd(1.0, Chain.from(simplex)(streamOrdering)))
          case None        => Some(Chain.from(simplex)(streamOrdering))
        }
      )
    }
  }
  def barcode: Seq[(Int, Double, Double)] = {
    simplexstream.foreach(step)
    state.cycleBirths
      .map { case (simplex, birth) =>
        ((simplex.size - 1, birth, Double.PositiveInfinity))
      }
      .toSeq
      .concat(barcodes)
  }
end NaivePersistentHomology
