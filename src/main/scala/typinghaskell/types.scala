package typinghaskell

import Ids._

// 4 Types

sealed abstract class Type {
  def kind: Kind
}

case class TyVar(id: Id, kind: Kind) extends Type

case class TyCon(id: Id, kind: Kind) extends Type

case class TyApp(left: Type, right: Type) extends Type {
  def kind = left.kind match {
    case KFun(_, k) => k
    case _ => throw new IllegalArgumentException("inconsistent type")
  }
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