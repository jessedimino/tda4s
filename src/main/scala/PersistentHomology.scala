package org.appliedtopology.tda4s

import collection.mutable
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.Set

object PersistentHomology:
  /**
  * Companion object for the NaivePersistentHomology class
  */
  def persistentHomology[T: Field as field](
      simplexStream: Iterator[Set[Int]],
      filtrationValues: PartialFunction[Set[Int], Double]
  ): Seq[(Int, Double, Double)] =
    NaivePersistentHomology(simplexStream, filtrationValues)(using field).barcode
    /**
    * method to run the persistent homology algorithm defined in the NaivePersistentHomology class
    *
    * @tparam T
    *  type parameter for the field, requires we have an instantiation of F[T]
    *
    * @param simplexStream
    *  iterable object of simplexes 
    *
    * @param filtrationValues
    *  partial function that takes a simplex as an input and returns the filtration value as a double
    */
end PersistentHomology

class NaivePersistentHomology[T: Field as field](simplexstream: Iterator[Set[Int]], filtrationValues: PartialFunction[Set[Int], Double]):
  /**
  * Class to run the persistent homology algorithm. It is naive in the sense that it is a textbook implementation with no
  * optimizations applied to it. It utilizes a form of row reduction to find the pivots of the boundary matrix,
  * which correspond to cycles in the underlying topology. 
  *
  * @tparam T
  *  type parameter for the field, requires we have an instantiation of F[T]
  *
  * @param simplexStream
  *  iterable object of simplexes 
  *
  * @param filtrationValues
  *  partial function that takes a simplex as an input and returns the filtration value as a double
  *
  */

  import field._
  def filtrationValue(simplex: Set[Int]): Double =
    filtrationValues.applyOrElse(simplex, _ => Double.PositiveInfinity)
    /**
    * Method for the filtration value of a simplex. Returns either the filtation value specified in filtrationValues or infinity
    * if it is not defined
    *
    * @param simplex
    *   The simplex used to compute the filtration value
    */
  def streamOrdering = Ordering.by(filtrationValue).orElseBy(SortedSet.from(_))(Ordering.Implicits.sortedSetOrdering)
    /**
    * Method to order the simplexes in the simplex stream. Should occur by the filtration values or the defailt set ordering in
    * Scala if the filtration isn't defined.
    */
  case class State(
      cycleBasis: Map[Set[Int], Chain[T]],
      boundaryBasis: Map[Set[Int], Chain[T]],
      coboundaryLookup: Map[Set[Int], Chain[T]],
      cycleBirths: Map[Set[Int], Double]
  )
  var state = State(Map.empty, Map.empty, Map.empty, Map.empty)
  val barcodes = mutable.ListBuffer.empty[(Int, Double, Double)]
  def step(simplex: Set[Int]): Unit = if (simplex.size <= 1) {
    val birth = filtrationValue(simplex)
    state = state.copy(
      cycleBasis = state.cycleBasis + (simplex -> Chain(
        SortedMap(simplex -> one)(using streamOrdering)
      )),
      cycleBirths = state.cycleBirths + (simplex -> birth)
    )
  } else {
    val simplexBoundary: Chain[T] = Chain.boundary(simplex)(streamOrdering)(using field)
    val (boundaryModBoundaries, boundaryReductionLog) = Chain.reduce(simplexBoundary, state.boundaryBasis)
    if (boundaryModBoundaries.isZero) {
      // then ∂s is already a boundary, say the boundary of z
      // hence ∂(s-z) = ∂s - ∂z = 0
      // so s-z is a new cycle
      val cycle = // look through reduction reductionHistory, read off coboundaries and scalingFactor and add
        boundaryReductionLog
          .foldLeft(Chain.from(simplex)(streamOrdering)) { case (chain, (spx, coeff)) =>
            chain.scaleAdd(chain.field.neg(coeff.asInstanceOf[chain.field.F]), state.coboundaryLookup(spx))
          }

      val (cycleModCycles, _) = Chain.reduce(cycle, state.cycleBasis)
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
            chain.scaleAdd(chain.field.neg(coeff.asInstanceOf[chain.field.F]), state.coboundaryLookup(spx))
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
