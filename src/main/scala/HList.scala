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

  implicit def index0[H, T <: HList](list : H :: T): HListView0[H, T] = {
    new HListView0[H, T](list)
  }
  implicit def indexN[H, T <: HList, Prev <: IndexedView](list : H :: T)(
    implicit indexTail: T => Prev): HListViewN[H, Prev] ={
    new HListViewN[H, Prev](list.head, indexTail(list.tail))
  }
}

sealed trait HList {
  type FullType <: HList
  type Expand[IfHCons <: Up, IfHNil <: Up, Up] <: Up
  type AsInitAndLast <: InitAndLastView
  type Append[T] <: AppendView
  type Join[H <: HList] <: JoinView
  type ViewAt[Idx <: Nat] <: IndexedView
  def :::[T <: HList](t: T)( implicit in: (T, FullType) => T#Join[FullType]):
    T#Join[FullType]#Joined
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
  type ViewAt[N <: Nat] = N#Expand[
    ({type Z[P <: Nat] = HListViewN[H, T#ViewAt[P]]})#Z,
    HListView0[H, T],
    IndexedView
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
  def ViewAt[Idx <: Nat](implicit in: FullType => FullType#ViewAt[Idx]) = in(this)
  override def toString: String = s"${head} :: ${tail}"
}

final class HNil extends HList {
  type FullType = HNil
  type Expand[IfHCons <: Up, IfHNil <: Up, Up] = IfHNil
  def ::[T](v: T) = HCons(v, this)
  type Append[T] = AppendView0[T]
  def append[T](t: T) = new AppendView0[T](t)
  type Join[H <: HList] = JoinView0[H]
  def Join[T <: HList](t: T) = new JoinView0[T](t)
  def :::[T <: HList](t: T)(implicit in: (T, FullType) => T#Join[FullType])
    = in(t, this).get
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

sealed trait IndexedView {
  import HList._
  type Before <: HList
  type After <: HList
  type At
  def fold[R](f: (Before, At, After) => R): R
  def get = fold( (_, value, _) => value)

  def remove(implicit in: (Before, After) => Before#Join[After]) = fold {
    (before, _, after) => in(before, after).get
  }

  def insertBefore[B](x: B)(implicit in: (Before, B :: At :: After) =>
    Before#Join[B::At::After]) = fold {
    (before, current, after) => in(before, HCons(x, HCons(current, after))).get
  }

  def insertAfter[B](x: B)(implicit in: (Before, At :: B :: After) =>
    Before#Join[At::B::After]) = fold {
    (before, current, after) => in(before, HCons(current, HCons(x, after))).get
  }

  def replace[B](x: B)(implicit in: (Before, B :: After) =>
    Before#Join[B::After]) = fold {
    (before, _, after) => in(before, HCons(x, after)).get
  }
}

class HListView0[H, T <: HList](val list : HCons[H, T]) extends IndexedView {
  import HList._
  type Before = HNil
  type After = T
  type At = H
  def fold[R](f: (Before, At, After) => R) : R = f(HNil, list.head, list.tail)
}

final class HListViewN[H, NextIdxView <: IndexedView](h: H, next: NextIdxView) extends IndexedView {
  type Before = HCons[H, NextIdxView#Before]
  type At = NextIdxView#At
  type After = NextIdxView#After

  def fold[R](f: (Before, At, After) => R) : R = next.fold(
    (before, at, after) => f(HCons(h, before), at, after)
  )
}

sealed trait Nat {
  type Expand[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] <: Up
}

object Nat{
  sealed trait _0 extends Nat {
    type Expand[NonZero[N <: Nat] <: Ret, IfZero <: Ret, Ret] = IfZero
  }
  sealed trait Succ[Prev <: Nat] extends Nat {
    type Expand[NonZero[N <: Nat] <: Ret, IfZero <: Ret, Ret] = NonZero[Prev]
  }
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]
  type _10 = Succ[_9]
  type _11 = Succ[_10]
  type _12 = Succ[_11]
  type _13 = Succ[_12]
  type _14 = Succ[_13]
  type _15 = Succ[_14]
  type _16 = Succ[_15]
  type _17 = Succ[_16]
  type _18 = Succ[_17]
  type _19 = Succ[_18]
  type _20 = Succ[_19]
  type _21 = Succ[_20]
  type _22 = Succ[_21]
  type _23 = Succ[_22]
  type _24 = Succ[_23]
  type _25 = Succ[_24]
  type _26 = Succ[_25]
  type _27 = Succ[_26]
  type _28 = Succ[_27]
  type _29 = Succ[_28]
  type _30 = Succ[_29]
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