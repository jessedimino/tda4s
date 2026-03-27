package org.appliedtopology.tda4s

import scala.collection.SortedSet

case class Simplex(vertices : SortedSet[Int]):
  def - (vertex: Int): Simplex = Simplex(vertices.diff(SortedSet(vertex)))
  def dimension: Int = vertices.size-1

object Simplex:
  def apply(vertices: Int*): Simplex = Simplex(SortedSet(vertices*))
  def from(vertices: Seq[Int]): Simplex = Simplex(SortedSet(vertices*))
  def unapply(simplex: Simplex): Option[SortedSet[Int]] = Some(simplex.vertices)
  def unapplySeq(simplex: Simplex): Option[Seq[Int]] = Some(simplex.vertices.toSeq)
