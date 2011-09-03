package typinghaskell

// 5 Substitutions

case class Subst(mappings: Seq[(TyVar, Type)]) {
  
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

object Subst {
  
  val nullSubst = Subst(Nil)
  
  def singletonSubst(v: TyVar, t: Type) = Subst(List((v, t)))
}

trait Types[T] {
  def apply(subst: Subst): T
  def tv: List[TyVar]
}

