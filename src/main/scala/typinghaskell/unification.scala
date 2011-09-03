package typinghaskell

import Subst._

// 6 Unification and Matching

object Unification {
  
  def mgu(left: Type, right: Type): Option[Subst] = (left, right) match {
    case (TyApp(l1, r1), TyApp(l2, r2)) => for {
      s1 <- mgu(l1, l2)
      s2 <- mgu(r1.apply(s1), r2.apply(s1))
    } yield s2 @@ s1
    
    case (v: TyVar, t) => varBind(v, t)
    
    case (t, v: TyVar) => varBind(v, t)
    
    case (tc1: TyCon, tc2: TyCon) if tc1 == tc2 => Some(nullSubst)
    
    case _ => None
  }
  
  def varBind(v: TyVar, t: Type): Option[Subst] =
    if (t == v)
      Some(nullSubst)
    else if (t.tv contains v)
      None // occurs check failed
    else if (v.kind != t.kind)
      None // kinds do not match
    else
      Some(v +-> t)
  
  def oneWayMatch(left: Type, right: Type): Option[Subst] = (left, right) match {
    case (TyApp(l1, r1), TyApp(l2, r2)) => for {
      s1 <- oneWayMatch(l1, l2)
      s2 <- oneWayMatch(r1, r2)
      s3 <- s1 merge s2
    } yield s3
    
    case (v: TyVar, t) if v.kind == t.kind => Some(v +-> t)
    
    case (tc1: TyCon, tc2: TyCon) if tc1 == tc2 => Some(nullSubst)
    
    case _ => None
  }
}