package jude

sealed trait Expression

sealed trait Type[L <: LT](val lt: L) extends Expression:

  override def toString(): String =
    this match
      case Type.AtomT(lt) => s"Atom$lt"
      case Type.PairT.SameT(l, t) => s"($t, $t)$l"
      case Type.PairT.DiffT(l, ta, tb) => s"($ta, $tb)"
      case _ => throw new Exception("Fooo")


object Type {
  case class AtomT[L <: LT](override val lt: L) extends Type[L](lt)
  sealed trait PairT[L <: LT] extends Type[L]
  object PairT {
    case class SameT[L <: LT,
                    LA <: LT, A <: Type[LA]](override val lt: L, ta: A) extends PairT[L]
                                                                           with Type[L](lt)
    case class DiffT[L <: LT,
                    LA <: LT, A <: Type[LA],
                    LB <: LT, B <: Type[LB]](override val lt: L, ta: A, tb: B) extends PairT[L]
                                                                                  with Type[L](lt)
  }
}

import Type._
import Value._

sealed trait Value[L <: LT, T <: Type[L]](val t: T) extends Expression:

  def toValueDescription: String =
    this match
      case Atom(name) => s"^${name}"
      case Pair.Same(v) => s"(${v.toValueDescription}, ${v.toValueDescription})"
      case Pair.Diff(va, vb) => s"(${va.toValueDescription}, ${vb.toValueDescription})"
      case _ => throw new Exception("Fooo")
      // case otherwise  => super.toString

  override def toString: String =
    s"${toValueDescription}: $t"

object Value {

  case class Atom[L <: LT]
    (name: String)(using l: L) extends Value[L, AtomT[L]](AtomT[L](l))

  sealed trait Pair[L <: LT, T <: Type[L]] extends Value[L, T]
  object Pair {
    case class Same[L <: LT,
                    LA <: LT, TA <: Type[LA], VA <: Value[LA, TA]](va: VA)(using l: L) extends Pair[L, PairT.SameT[L, LA, TA]]
                                                                                          with Value[L, PairT.SameT[L, LA, TA]](PairT.SameT(l, va.t))
    case class Diff[L <: LT,
                    LA <: LT, TA <: Type[LA], VA <: Value[LA, TA],
                    LB <: LT, TB <: Type[LB], VB <: Value[LB, TB]](va: VA, vb: VB)(using l: L) extends Pair[L, PairT.DiffT[L, LA, TA, LB, TB]]
                                                                                          with Value[L, PairT.DiffT[L, LA, TA, LB, TB]](PairT.DiffT(l, va.t, vb.t))
  }

}

object DSL {

  import scala.language.dynamics

  class AtomBuilder[L <: LT](using l: L) extends Dynamic:

    import Value._

    def selectDynamic(atomName: String): Atom[L] =
      Atom[L](atomName)

    def apply[LA <: LT, TA <: Type[LA], VA <: Value[LA, TA]]
      (va: VA, ignoreme: VA): Pair.Same[L, LA, TA, VA] =
        Pair.Same[L, LA, TA, VA](va)

    def apply[LA <: LT, TA <: Type[LA], VA <: Value[LA, TA],
              LB <: LT, TB <: Type[LB], VB <: Value[LB, TB]](va: VA, vb: VB): Pair[L, PairT.DiffT[L, LA, TA, LB, TB]] =
        Pair.Diff[L, LA, TA, VA, LB, TB, VB](va, vb)

  def ^[L <: LT](using l: L): AtomBuilder[L] = AtomBuilder[L]()

}

@main def hello: Unit =

  val program = {
    import DSL._
    import scala.language.postfixOps

    ^[A](^[B]foo, ^[C]foo)
  }

  println("PROGRAM")
  println("-------")
  println(program)



trait LT

trait A extends LT
given ALT: A with
  override val toString: String = "'A"
trait B extends LT
given BLT: B with
  override val toString: String = "'B"
trait C extends LT
given CLT: C with
  override val toString: String = "'C"
trait E extends LT
given ELT: E with
  override val toString: String = "'E"
trait F extends LT
given FLT: F with
  override val toString: String = "'F"
trait G extends LT
given GLT: G with
  override val toString: String = "'G"
trait H extends LT
given HLT: H with
  override val toString: String = "'H"
trait I extends LT
given ILT: I with
  override val toString: String = "'I"
trait J extends LT
given JLT: J with
  override val toString: String = "'J"
trait K extends LT
given KLT: K with
  override val toString: String = "'K"
trait L extends LT
given LLT: L with
  override val toString: String = "'L"
trait M extends LT
given MLT: M with
  override val toString: String = "'M"
trait N extends LT
given NLT: N with
  override val toString: String = "'N"
trait O extends LT
given OLT: O with
  override val toString: String = "'O"
trait P extends LT
given PLT: P with
  override val toString: String = "'P"
trait Q extends LT
given QLT: Q with
  override val toString: String = "'Q"
trait R extends LT
given RLT: R with
  override val toString: String = "'R"
trait S extends LT
given SLT: S with
  override val toString: String = "'S"
trait T extends LT
given TLT: T with
  override val toString: String = "'T"
trait U extends LT
given ULT: U with
  override val toString: String = "'U"
trait V extends LT
given VLT: V with
  override val toString: String = "'V"
trait W extends LT
given WLT: W with
  override val toString: String = "'W"
trait X extends LT
given XLT: X with
  override val toString: String = "'X"
trait Y extends LT
given YLT: Y with
  override val toString: String = "'Y"
trait Z extends LT
given ZLT: Z with
  override val toString: String = "'Z"


