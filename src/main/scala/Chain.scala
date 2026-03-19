package org.appliedtopology.tda4s

import Ordering.Implicits.*
import scala.Predef.->
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.{Set,Map}

case class Chain(entries: SortedMap[Set[Int], Double]):
  def scale(scale: Double): Chain = Chain(entries.collect[Set[Int],Double]{
    case (k,v) => (k, v * scale)
  }(using entries.ordering))
  def scaleAdd(scale: Double, other: Chain): Chain =
    Chain(other.entries.foldLeft(entries) { case (result, (s, v)) =>
      result.getOrElse(s, 0.0) match {
        case v0: Double =>
          v0 + scale * v match {
            case v1 if math.abs(v1) < 1e-10 => result.removed(s)
            case v1                         => result.updated(s, v1)
          }
      }
    })
  def isZero: Boolean = entries.values.forall(math.abs(_) < 1e-10)
  def leading: Option[(Set[Int], Double)] = entries.lastOption

object Chain:
  def from(simplex: Set[Int])(ordering: Ordering[Set[Int]]): Chain = Chain(SortedMap((simplex, 1.0))(using ordering))
  def boundary(simplex: Set[Int])(ordering: Ordering[Set[Int]]): Chain = {
    val vertices = simplex.toSeq.sorted
    val faces = vertices.map(i => simplex-i)
    val values = Iterator.iterate(1.0)(-_)
    val pairs = faces.zip(values).toSeq
    val entries = SortedMap(pairs: _*)(using ordering)
    Chain(entries)
  }

  //@tailrec
  def reduceBy(chain: Chain, basis: Map[Set[Int], Chain], log: Map[Set[Int], Double] = Map.empty): (Chain, Map[Set[Int], Double]) = {
    var retchain : Chain = chain
    var retlog : Map[Set[Int],Double] = Map.empty
    var done : Boolean = false
    while (!done) {
      done = true
      for ((basisSimplex, basisChain) <- basis) {
        var retTerm = retchain.entries.get(basisSimplex)
        retTerm match {
          case None => ()
          case Some(chainCoeff) => {
            val coeff = chainCoeff / basisChain.entries(basisSimplex)
            retchain = retchain.scaleAdd(-coeff, basisChain)
            retlog = retlog + ((basisSimplex, coeff))
            done = false
          }
        }
      }
    }
    (retchain, retlog)
  }
