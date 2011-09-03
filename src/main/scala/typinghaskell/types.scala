package typinghaskell

import Ids._

// 4 Types

sealed abstract class Type extends Types[Type] {
  def kind: Kind
  
  def apply(subst: Subst) = this
  
  def tv: List[TyVar] = Nil
}

case class TyVar(id: Id, kind: Kind) extends Type {
  
  def +->(t: Type) = Subst.singletonSubst(this, t)
  
  override def apply(subst: Subst) = subst.lookup(this).getOrElse(this)
  
  override def tv: List[TyVar] = List(this)
}

case class TyCon(id: Id, kind: Kind) extends Type

case class TyApp(left: Type, right: Type) extends Type {
  def kind = left.kind match {
    case KFun(_, k) => k
    case _ => throw new IllegalArgumentException("inconsistent type")
  }
  
  override def apply(subst: Subst) = TyApp(left.apply(subst), right.apply(subst))
  
  override def tv: List[TyVar] = left.tv union right.tv
}

case class TyGen(num: Int) extends Type {
  def kind =
    throw new UnsupportedOperationException("generic type variables have no kind")
}

object TypeHelpers {

  val unitType = TyCon("()", Star)
  val charType = TyCon("Char", Star)
  val intType = TyCon("Int", Star)
  val floatType = TyCon("Float", Star)
  val doubleType = TyCon("Double", Star)

  val listType = TyCon("[]", KFun(Star, Star))
  val arrowType = TyCon("->", KFun(Star, KFun(Star, Star)))
  val pairType = TyCon("(,)", KFun(Star, KFun(Star, Star)))

  def fn(a: Type, b: Type) = TyApp(TyApp(arrowType, a), b)
  def list(t: Type) = TyApp(listType, t)
  def pair(a: Type, b: Type) = TyApp(TyApp(pairType, a), b)
  
}