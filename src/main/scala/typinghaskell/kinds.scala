package typinghaskell

// 3 Kinds

sealed abstract class Kind

case object Star extends Kind

case class KFun(left: Kind, right: Kind) extends Kind
