package org.appliedtopology.tda4s

import collection.mutable
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.Set

object PersistentHomology:
  def persistentHomology(
      simplexStream: Iterator[Set[Int]],
      filtrationValues: PartialFunction[Set[Int], Double]
  ): Seq[(Int, Double, Double)] =
    NaivePersistentHomology(simplexStream, filtrationValues).barcode
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
    val simplexBoundary = Chain.boundary(simplex)(streamOrdering)
    val (boundaryModBoundaries, boundaryReductionLog) = Chain.reduceBy(simplexBoundary, state.boundaryBasis)
    if (boundaryModBoundaries.isZero) {
      // then ∂s is already a boundary, say the boundary of z
      // hence ∂(s-z) = ∂s - ∂z = 0
      // so s-z is a new cycle
      val cycle = // look through reduction reductionHistory, read off coboundaries and scalingFactor and add
        boundaryReductionLog
          .foldLeft(Chain.from(simplex)(streamOrdering)) { case (chain, (spx, coeff)) =>
            chain.scaleAdd(-coeff, state.coboundaryLookup(spx))
          }

      val (cycleModCycles, _) = Chain.reduceBy(cycle, state.cycleBasis)
      val birthTime = filtrationValue(simplex)

      state = cycleModCycles.leading match {
        case Some((spx, _)) =>
          state.copy(
            cycleBasis = state.cycleBasis + (spx -> cycleModCycles),
            cycleBirths = state.cycleBirths + (spx -> birthTime)
          )
        case None: Option[(Set[Int], Double)] => state
      }
    } else {
      // since ∂s is not a boundary already, it has to be a cycle
      // reducing mod cycles will tell us which cycle just became a boundary with the addition of simplex
      // val (dsModZ, reductionZ) = Chain.reduceBy(boundaryModBoundaries, state.cycleBasis)
      // assert(dsModZ.isZero)
      val dyingCycleLeadingSimplex = boundaryModBoundaries.simplexCoefficients.keys
        .filter(k => state.cycleBasis.contains(k))
        .max(using streamOrdering)
      val updatedCoboundary = // look through reduction reductionHistory, read off coboundaries and scalingFactor and add
        boundaryReductionLog
          .foldLeft(Chain.from(simplex)(streamOrdering)) { case (chain, (spx, coeff)) =>
            chain.scaleAdd(-coeff, state.coboundaryLookup(spx))
          }
      if (math.abs(filtrationValue(simplex) - state.cycleBirths(dyingCycleLeadingSimplex)) > 1e-15)
        barcodes.append((dyingCycleLeadingSimplex.size - 1, state.cycleBirths(dyingCycleLeadingSimplex), filtrationValue(simplex)))
      assert(simplex.size == dyingCycleLeadingSimplex.size + 1)
      state = state.copy(
        cycleBasis = state.cycleBasis - dyingCycleLeadingSimplex,
        boundaryBasis = state.boundaryBasis + (boundaryModBoundaries.leading.get._1 -> boundaryModBoundaries),
        cycleBirths = state.cycleBirths - dyingCycleLeadingSimplex,
        coboundaryLookup = state.coboundaryLookup + (dyingCycleLeadingSimplex -> updatedCoboundary)
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
