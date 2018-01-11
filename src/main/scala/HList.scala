/**
  * Created by river on 17/4/16.
  */

object HList {
  type ::[H, T <: HList] = HCons[H, T]
  val :: = HCons
  val HNil = new  HNil

  implicit def initLast0[H](x : H :: HNil) = new InitAndLastViewHNil[H](x.head)
  implicit def initLastN[H, T <: HList, Prev <: InitAndLastView](
        x: H :: T)(implicit prev: T => Prev) = {
    new InitAndLastViewHCons[H, Prev](x.head, prev(x.tail))
  }

  implicit def append0[H, T](x : H :: HNil, t: T) =
    new AppendViewN(x.head, new AppendView0(t))
  implicit def appendN[H, T <: HList, T1, Prev <: AppendView]
      (x : H :: T, t: T1)(implicit prev: (T, T1) => Prev) =
    new AppendViewN(x.head, prev(x.tail, t))

  implicit def join0[T <: HList](x : HNil, t: T) = new JoinView0(t)
  implicit def joinN[H, T1 <: HList, T <: HList, Prev <: JoinView](
      x : H :: T1, t: T)(implicit prev : (T1, T) => Prev)
    = new JoinViewN(x.head, prev(x.tail, t))

}

sealed trait HList {
  type Expand[IfHCons <: Up, IfHNil <: Up, Up] <: Up
  type AsInitAndLast <: InitAndLastView
  type Append[T] <: AppendView
  type Join[H <: HList] <: JoinView
}

final case class HCons[H, T <: HList](head: H, tail: T) extends HList {
  import HList._
  type Expand[IfHCons <: Up, IfHNil <: Up, Up] = IfHCons
  type FullType = H :: T
  type AsInitAndLast = T#Expand[
    InitAndLastViewHCons[H, T#AsInitAndLast],
    InitAndLastViewHNil[H],
    InitAndLastView
    ]
  type Append[T1] = AppendViewN[H,T#Append[T1]]
//  type Append[T1] = FullType#Expand[
//    AppendViewN[H,T#Append[T1]],
//    AppendView0[T1],
//    AppendView
//    ]
  type Join[H1 <: HList] = T#Expand[
    JoinViewN[H, T#Join[H1]],
    JoinViewN[H, JoinView0[H1]],
    JoinView
    ]
  def asInitAndLast(implicit in: FullType => FullType#AsInitAndLast): FullType#AsInitAndLast = in(this)
  def init(implicit in: FullType => FullType#AsInitAndLast) = in(this).init
  def last(implicit in: FullType => FullType#AsInitAndLast) = in(this).last
  def append[T](t: T)(implicit in: (FullType, T) => FullType#Append[T]) = in(this, t)
  def Join[T <: HList](t: T)(
    implicit in: (FullType, T) => FullType#Join[T]
  ) = in(this, t)
  def :::[T <: HList](t: T)(
    implicit in: (T, FullType) => T#Join[FullType]
  ) = in(t, this).get
  def ::[T](v: T) = HCons(v, this)
  override def toString: String = s"${head} :: ${tail}"
}

final class HNil extends HList {
  type Expand[IfHCons <: Up, IfHNil <: Up, Up] = IfHNil
  def ::[T](v: T) = HCons(v, this)
  type Append[T] = AppendView0[T]
  def append[T](t: T) = new AppendView0[T](t)
  type Join[H <: HList] = JoinView0[H]
  def Join[T <: HList](t: T) = new JoinView0[T](t)
  def :::[T <: HList](t: T) = t
  override def toString: String = "HNil"
}

trait InitAndLastView {
  type init <: HList
  type last
  def last: last
  def init: init
}

class InitAndLastViewHNil[H](val x: H) extends InitAndLastView {
  import HList._
  type init = HNil
  type last = H
  def last = x
  def init = HNil
}

class InitAndLastViewHCons[H, NextLastView <: InitAndLastView](
    val x: H, val v: NextLastView) extends InitAndLastView {
  import HList._
  type init = H :: NextLastView#init
  type last = v.last
  def last = v.last
  def init = HCons(x, v.init) // 为何这里不能写 x :: v.init
}

sealed trait AppendView {
  type Appended <: HList
  def get: Appended
}

class AppendView0[T](val t : T) extends AppendView {
  import HList._
  type Appended = T :: HNil
  def get = t :: HNil
}

class AppendViewN[H, A <: AppendView](val h: H, val a: A) extends AppendView {
  import HList._
  type Appended = H :: A#Appended
  def get = HCons(h, a.get)
}

sealed trait JoinView {
  type Joined <: HList
  def get: Joined
}

class JoinView0[T <: HList](val t: T) extends JoinView {
  type Joined = T
  def get = t
}

class JoinViewN[H, NextJoinView <: JoinView](
    val h: H, x: NextJoinView) extends JoinView {
  import HList._
  type Joined = H :: NextJoinView#Joined
  def get = HCons(h, x.get)
}

object Main1 extends App {
  import HList._
  val x = "Hello world!" :: 42 :: true :: HNil
  val t = "Hello word!" :: x
  val y: x.AsInitAndLast = initLastN(x)
  // val z: y.last = 3.5 // compile error
//   val z: y.last = false // compile error
//   val z: x.AsInitAndLast#last = false
}