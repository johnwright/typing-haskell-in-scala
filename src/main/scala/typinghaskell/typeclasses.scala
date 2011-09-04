package typinghaskell

import Ids._
import Subst._

// 7 Type Classes, Predicates and Qualified Types

case class Qual[T <% Types[T]](preds: List[Pred], qualified: T)

sealed abstract class Pred

case class IsIn(id: Id, t: Type) extends Pred

object Qual {
  
  import Pred._
  
  implicit def preds2Qual(preds: List[Pred]) = new {
    def :=>[T <% Types[T]](qualified: T) = Qual(preds, qualified)
  }
  
  implicit def qual2Types[T <% Types[T]](q: Qual[T]): Types[Qual[T]] = new Types[Qual[T]] {
    def apply(subst: Subst) = 
      q.preds.apply(subst) :=> q.qualified.apply(subst)
    
    def tv: List[TyVar] = q.preds.tv union q.qualified.tv
  }
}

object Pred {
  
  implicit def pred2Types(p: Pred): Types[Pred] = new Types[Pred] {
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

object Class {
  
  import Qual._
  import Pred._
  
  case class Class(supers: List[Id], insts: List[Inst])
  
  type Inst = Qual[Pred]
  
  case class ClassEnv(classes: Id => Option[Class], defaults: List[Type]) {
    
    def supers(id: Id) = classes(id).map(_.supers).getOrElse(Nil)
    
    def insts(id: Id) = classes(id).map(_.insts).getOrElse(Nil)
    
    def modify(id: Id, c: Class) =
      this.copy(classes = (otherId: Id) =>
        if (id == otherId) Some(c) else classes(otherId))
  }
  
  val initialEnv = ClassEnv(classes = (id: Id) => None,
                            defaults = List(TypeHelpers.intType,
                                            TypeHelpers.doubleType))
  
  type EnvTransformer = ClassEnv => Option[ClassEnv]
  
  implicit def envTransformer2Composable(f: EnvTransformer) = new {
    def <:>(g: EnvTransformer) = (env1: ClassEnv) => for {
      env2 <- f(env1)
      env3 <- g(env2)
    } yield env3
  }
  
  def addClass(id: Id, supers: Id*): EnvTransformer = (env: ClassEnv) =>
    if (env.classes(id).isDefined)
      None // class already defined
    else if (supers.exists(superId => env.classes(superId).isEmpty))
      None // superclass not defined
    else
      Some(env.modify(id, Class(supers.toList, Nil)))
  
  def addPreludeClasses = addCoreClasses <:> addNumClasses
  
  def addCoreClasses = (addClass("Eq")
                    <:> addClass("Ord", "Eq")
                    <:> addClass("Show")
                    <:> addClass("Read")
                    <:> addClass("Bounded")
                    <:> addClass("Enum")
                    <:> addClass("Functor")
                    <:> addClass("Monad"))
  
  def addNumClasses = (addClass("Num", "Eq", "Show")
                   <:> addClass("Real", "Num", "Ord")
                   <:> addClass("Fractional", "Num")
                   <:> addClass("Integral", "Real", "Enum")
                   <:> addClass("RealFrac", "Real", "Fractional")
                   <:> addClass("Floating", "Fractional")
                   <:> addClass("RealFloat", "RealFrac", "Floating"))
  
  
  def addInst(inst: Inst): EnvTransformer = (env: ClassEnv) => inst.qualified match {
    case p @ IsIn(id, _) =>
      val insts = env.insts(id)
      val qs = insts.map(_.qualified)
      val c = Class(env.supers(id), (inst.preds :=> (p: Pred)) :: insts)
      
      if (env.classes(id).isEmpty)
        None // no class for instance
      else if (qs.exists(overlap(p, _)))
        None // overlapping instance
      else
        Some(env.modify(id, c))
  }
  
  def overlap(p: Pred, q: Pred) =
    mguPred(p, q).isDefined
}