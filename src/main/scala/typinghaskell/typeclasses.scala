package typinghaskell

import Ids._
import Subst._

// 7 Type Classes, Predicates and Qualified Types

case class Qual[T <% Types[T]](preds: List[Pred], t: T)

sealed abstract class Pred

case class IsIn(id: Id, t: Type) extends Pred

object Qual {
  
  import Pred._
  
  implicit def preds2Qual(preds: List[Pred]) = new {
    def :=>[T <% Types[T]](t: T) = Qual(preds, t)
  }
  
  implicit def qual2Types[T <% Types[T]](qual: Qual[T]): Types[Qual[T]] = new Types[Qual[T]] {
    def apply(subst: Subst) = 
      qual.preds.apply(subst) :=> qual.t.apply(subst)
    
    def tv: List[TyVar] = qual.preds.tv union qual.t.tv
  }
}

object Pred {
  
  implicit def pred2Type(p: Pred): Types[Pred] = new Types[Pred] {
    def apply(subst: Subst) = p match {
      case IsIn(id, t) => IsIn(id, t.apply(subst))
    }
  
    def tv: List[TyVar] = p match {
      case IsIn(_, t) => t.tv
    }
  }
  
  def lift(f: (Type, Type) => Option[Subst])(left: Pred, right: Pred): Option[Subst] = (left, right) match {
    case (IsIn(id1, t1), IsIn(id2, t2)) if id1 == id2 => f(t1, t2)
    case _                                            => None // classes differ
  }
  
  val mguPred = lift(Unification.mgu _) _
  
  val oneWayMatchPred = lift(Unification.oneWayMatch _) _
}