package org.tygus.suslik.logic

import org.tygus.suslik.language.Expressions._
import org.tygus.suslik.language.{BoolType, HasExpressions, PrettyPrinting, SSLType}

import scala.collection.immutable.SortedSet


case class PFormula(conjuncts: SortedSet[Expr]) extends PrettyPrinting with HasExpressions[PFormula] {
  def toExpr: Expr = if (conjuncts.isEmpty) eTrue else conjuncts.reduce(_ && _)

  override def pp: String = toExpr.pp

  def subst(sigma: Subst): PFormula = PFormula(conjuncts.flatMap(_.subst(sigma).flatten))
  def substUnknown(sigma: UnknownSubst): PFormula = PFormula(conjuncts.map(_.substUnknown(sigma)))
  def substUnknown(unknown: Unknown, expr: Expr): PFormula = substUnknown(Map(unknown -> expr))

  def resolveOverloading(gamma: Gamma): PFormula = PFormula(conjuncts.map(_.resolveOverloading(gamma)))

  def resolve(gamma: Gamma): Option[Gamma] = {
    conjuncts.foldLeft[Option[Map[Var, SSLType]]](Some(gamma))((go, c) => go match {
      case None => None
      case Some(g) => c.resolve(g, Some(BoolType)) match {
        case None => throw SepLogicException(s"Resolution error in conjunct: ${c.pp}")
        case Some(g1) => Some(g1)
      }
    })
  }

  // Collect certain sub-expressions
  def collect[R <: Expr](p: Expr => Boolean): Set[R] = {
    conjuncts.map(_.collect(p)).foldLeft(Set.empty[R])(_ ++ _)
  }

  def unknowns: Set[Unknown] = collect[Unknown](_.isInstanceOf[Unknown])
  // TODO separate outlives rels and blocked rels
  def outlivesRels: Set[(NamedLifetime, NamedLifetime)] = {
    var rels = this.collect[BinaryExpr](p => p.isInstanceOf[BinaryExpr] && p.asInstanceOf[BinaryExpr].op == OpOutlived)
      .map(p => p.left.asInstanceOf[NamedLifetime] -> p.right.asInstanceOf[NamedLifetime])
    var changed = true
    while (changed) {
      changed = false
      val newRels = rels.flatMap(rel => rels.filter(_._1 == rel._2).map(other => (rel._1, other._2)))
      if (!newRels.forall(rels(_))) {
        rels = rels ++ newRels
        changed = true
      }
    }
    rels
  }

  // Add h to chunks (multiset semantics)
  def &&(c: Expr): PFormula = PFormula(conjuncts ++ c.conjuncts.toSet)

  // Add all chunks from other (multiset semantics)
  def &&(other: PFormula): PFormula = PFormula(conjuncts ++ other.conjuncts)

  // Remove h from this formula (multiset semantics)
  def -(c: Expr): PFormula = PFormula(conjuncts -- c.conjuncts.toSet)

  // Remove all chunks present in other (multiset semantics)
  def -(other: PFormula): PFormula = PFormula(conjuncts -- other.conjuncts)

  def ==> (other: PFormula): Expr = this.toExpr ==> other.toExpr

  def ==> (other: Expr): Expr = this.toExpr ==> other

  def size: Int = conjuncts.map(_.size).sum

  // Subset of my conjuncts that cannot influence variables vs
  // (i.e. does directly contain those variables or any variables that participate in the same conjunct with them)
  def indepedentOf(vs: Set[Var]): PFormula = {
    val (newUnused, newUsed) = conjuncts.partition(_.vars.intersect(vs).isEmpty)
    if (newUsed.isEmpty) this
    else copy(conjuncts = newUnused).indepedentOf(vs ++ newUsed.toList.flatMap(_.vars))
  }

}

object PFormula {
  def apply(cs: Set[Expr]): PFormula = PFormula(SortedSet[Expr]() ++ cs)

  def apply(c: Expr): PFormula = PFormula(SortedSet[Expr]() ++ c.conjuncts)
}
