/**
  * Created by river on 17/4/16.
  */

object HList {
  sealed trait HList {
    type AsInitAndLast <: InitAndLastView
    type Append[T] <: AppendView
    type Expand[IfHCons <: Up, IfHNil <: Up, Up] <: Up
  }

  final case class HCons[H, T <: HList](head: H, tail: T) extends HList {
    type FullType = H :: T

    type Expand[IfHCons <: Up, IfHNil <: Up, Up] = IfHCons

    type AsInitAndLast = T#Expand[
      InitAndLastViewHCons[H, T#AsInitAndLast],
      InitAndLastViewHNil[H],
      InitAndLastView
    ]

    type Append[T1] = AppendViewN[H,T#Append[T1]]

    def AsInitAndLast(implicit in: FullType => FullType#AsInitAndLast) = in(this)
    def Append[T](t: T)(implicit in: (FullType, T) => FullType#Append[T]) = in(this, t)

    def ::[T](v: T) = HCons(v, this)

    override def toString: String = s"${head} :: ${tail}"
  }

  final class HNil extends HList {

    type Expand[IfHCons <: Up, IfHNil <: Up, Up] = IfHNil

    type Append[T] = AppendView0[T]

    def Append[T](t: T) = new AppendView0[T](t)

    def ::[T](v: T) = HCons(v, this)

    override def toString: String = "HNil"
  }

  trait InitAndLastView {
    type init <: HList
    type last

    def last: last
    def init: init
  }

  class InitAndLastViewHNil[H](val x: H) extends InitAndLastView {
    type init = HNil
    type last = H
    def last = x
    def init = HNil
  }

  class InitAndLastViewHCons[H, NextLastView <: InitAndLastView](val x: H, val v: NextLastView
                                                                ) extends InitAndLastView {
    type init = H :: NextLastView#init
    type last = NextLastView#last
    def last = v.last
    def init = HCons(x, v.init) // 为何这里不能写 x :: v.init
  }

  implicit def initLast0[H](x : H :: HNil) = new InitAndLastViewHNil[H](x.head)
  implicit def initLastN[H, T <: HList, Prev <: InitAndLastView](x: H :: T)(implicit prev: T => Prev) = {
    new InitAndLastViewHCons[H, Prev](x.head, prev(x.tail))
  }

  sealed trait AppendView {
    type Appended <: HList

    def get: Appended
  }

  class AppendView0[T](val t : T) extends AppendView {
    type Appended = T :: HNil
    def get = t :: HNil
  }

  class AppendViewN[H, A <: AppendView](val h: H, val a: A) extends AppendView {
    type Appended = H :: A#Appended
    def get = HCons(h, a.get)
  }

  implicit def append0[H, T](x : H :: HNil, t: T) = new AppendViewN(x.head, new AppendView0(t))
  implicit def appendN[H, T <: HList, T1, Prev <: AppendView](x : H :: T, t: T1)(implicit prev: (T, T1) => Prev)
  = new AppendViewN(x.head, prev(x.tail, t))

  type ::[H, T <: HList] = HCons[H, T]
  val :: = HCons
  val HNil = new  HNil

}
