package typinghaskell

// 5 Substitutions

case class Subst(mappings: Seq[(TyVar, Type)]) {
  
  import Subst._
  
  def lookup(v: TyVar) =
    mappings.find { case (tv, _) => tv == v }.map { case (_, t) => t }
  
  def @@(that: Subst) = {
    val ms = for { (u, t) <- that.mappings } yield (u, t.apply(this))
    Subst(ms ++ this.mappings)
  }
  
  def merge(that: Subst): Option[Subst] = {
    val agree = (this.tyVars intersect that.tyVars).forall { v =>
      v.apply(this) == v.apply(that)
    }
    if (agree)
      Some(Subst(this.mappings ++ that.mappings))
    else
      None
  }
  
  def tyVars = mappings.map(_._1)
  
}

trait Types[T] {
  def apply(subst: Subst): T
  def tv: List[TyVar]
}

object Subst {
  
  val nullSubst = Subst(Nil)
  
  implicit def tyVar2SingletonSubst(v: TyVar) = new {
    def +->(t: Type) = Subst(List((v, t)))
  }
  
  implicit def type2Types(t: Type): Types[Type] = new Types[Type] {
    def apply(subst: Subst): Type = t match {
      case t: TyVar           => subst.lookup(t).getOrElse(t)
      case TyApp(left, right) => TyApp(left.apply(subst), right.apply(subst))
      case _                  => t
      
    }
    
    def tv: List[TyVar] = t match {
      case t: TyVar           => List(t)
      case TyApp(left, right) => left.tv union right.tv
      case _                  => Nil
    }
  }
  
  implicit def list2Types[T <% Types[T]](ts: List[T]) = new Types[List[T]] {
    def apply(subst: Subst) = ts.map(_.apply(subst))
    
    def tv: List[TyVar] = ts.flatMap(_.tv).distinct
  }
}