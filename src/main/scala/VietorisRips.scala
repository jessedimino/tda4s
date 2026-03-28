package org.appliedtopology.tda4s

import scala.collection.Set

trait VietorisRips(val metricSpace: MetricSpace[Int]):
  /**
  * Vietoris Rips trait for defining filtration values
  *
  * @param metricSpace
  *   metric space where the underlying metric is used to compute distances between points for the filtration values
  */
  def filtrationValues: PartialFunction[Set[Int], Double] = {
    //partial function that computes the filtration of a simplex
    // returns -infinity is the simplex is empty, 0 for a 0-dimensional simplex, and the largest pairwise distance for a higher dimensional simplex
    case spx if spx.isEmpty   => Double.NegativeInfinity
    case spx if spx.size == 1 => 0.0
    case spx =>
      spx.toSeq.combinations(2).map { case Seq(x, y) => metricSpace.distance(x, y) }.max
  }
  def simplicesInDimension(dimension: Int): Iterator[Set[Int]]
    //Iterator for simplexes
  def simplices(): Iterator[Set[Int]] = metricSpace.elements.indices.iterator.flatMap(simplicesInDimension)
    //Get the simplexes by iterating through the elements of the metric space and storing them in a flattened list

class NaiveVietorisRips(metricSpace: MetricSpace[Int]) extends VietorisRips(metricSpace):
  /**
  * Class for the Naive Vietoris Rips Stream formulation. Naive in the sense that no optimizations are applied to speed computation. It extends the Vietoris Rips Trait
  *
  * @param metricSpace
  *   metric space where the underlying metric is used to compute distances between points for the filtration values
  */
  override def simplicesInDimension(dimension: Int): Iterator[Set[Int]] =
    /**
    * Iterator object to generate all of the simplexes of a given dimension. Generates all of the combinations of a given dimension and computes the resulting filtration value by taking
    * the largest pairwise distance and sorting accordingly
    *
    * @param metricSpace
    *   metric space where the underlying metric is used to compute distances between points for the filtration values
    */
    metricSpace.elements
      .combinations(dimension + 1)
      .map(Set.from(_))
      .toSeq
      .sortBy(filtrationValues.applyOrElse(_, _ => Double.PositiveInfinity))
      .iterator

