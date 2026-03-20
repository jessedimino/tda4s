package org.appliedtopology.tda4s

import scala.collection.Set

trait VietorisRips(val metricSpace: MetricSpace[Int]):
  def filtrationValues: PartialFunction[Set[Int], Double] = {
    case spx if spx.isEmpty   => Double.NegativeInfinity
    case spx if spx.size == 1 => 0.0
    case spx =>
      spx.toSeq.combinations(2).map { case Seq(x, y) => metricSpace.distance(x, y) }.max
  }
  def simplicesInDimension(dimension: Int): Iterator[Set[Int]]
  def simplices(): Iterator[Set[Int]] = metricSpace.elements.indices.iterator.flatMap(simplicesInDimension)

class NaiveVietorisRips(metricSpace: MetricSpace[Int]) extends VietorisRips(metricSpace):
  override def simplicesInDimension(dimension: Int): Iterator[Set[Int]] =
    metricSpace.elements
      .combinations(dimension + 1)
      .map(Set.from(_))
      .toSeq
      .sortBy(filtrationValues.applyOrElse(_, _ => Double.PositiveInfinity))
      .iterator
