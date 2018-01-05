/**
  * Created by river on 17/4/16.
  */

object HList {
  sealed trait HList {
    type FullType <: HList

    type AsInitAndLast <: InitAndLastView
    type Append[T] <: AppendView
    type Join[H <: HList] <: JoinView
    type ViewAt[Idx <: Nat] <: IndexedView

    def Join[T <: HList](t: T)(implicit in: (FullType, T) => FullType#Join[T]): FullType#Join[T]

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

    def AsInitAndLast(implicit in: FullType => FullType#AsInitAndLast) = in(this)
    def Append[T](t: T)(implicit in: (FullType, T) => FullType#Append[T]) = in(this, t)
    def Join[T <: HList](t: T)(implicit in: (FullType, T) => FullType#Join[T]) = in(this, t)
    def :::[T <: HList](t: T)(implicit in: (T, FullType) => T#Join[FullType]) = in(t, this).get
    def ViewAt[Idx <: Nat](implicit in: FullType => FullType#ViewAt[Idx]) = in(this)

    def ::[T](v: T) = HCons(v, this)

    override def toString: String = s"${head} :: ${tail}"
  }

  final class HNil extends HList {
    type FullType = HNil

    type Expand[IfHCons <: Up, IfHNil <: Up, Up] = IfHNil

    type Append[T] = AppendView0[T]

    type Join[H <: HList] = JoinView0[H]

    def Append[T](t: T) = new AppendView0[T](t)

    def Join[T <: HList](t: T)(implicit in: (FullType, T) => FullType#Join[T]) = new JoinView0[T](t)

    def ::[T](v: T) = HCons(v, this)

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


  sealed trait JoinView {
    type Joined <: HList
    def get: Joined
  }

  class JoinView0[T <: HList](val t: T) extends JoinView {
    type Joined = T
    def get = t
  }

  class JoinViewN[H, NextJoinView <: JoinView](val h: H, x: NextJoinView) extends JoinView {
    type Joined = H :: NextJoinView#Joined
    def get = HCons(h, x.get)
  }

  implicit def join0[T <: HList](x : HNil, t: T) = new JoinView0[T](t)
  implicit def joinN[H, T1 <: HList, T <: HList, Prev <: JoinView](x : H :: T1, t: T)(implicit prev : (T1, T) => Prev)
  = new JoinViewN(x.head, prev(x.tail, t))

  sealed trait IndexedView {
    type Before <: HList
    type After <: HList
    type At
    def fold[R](f: (Before, At, After) => R): R
    def get = fold( (_, value, _) => value)

  }

  class HListView0[H, T <: HList](val list : H :: T) extends IndexedView {
    type Before = HNil
    type After = T
    type At = H
    def fold[R](f: (Before, At, After) => R) : R = f(HNil, list.head, list.tail)
  }

  final class HListViewN[H, NextIdxView <: IndexedView](h: H, next: NextIdxView) extends IndexedView {
    type Before = H :: NextIdxView#Before
    type At = NextIdxView#At
    type After = NextIdxView#After

    def fold[R](f: (Before, At, After) => R) : R = next.fold(
      (before, at, after) => f(HCons(h, before), at, after)
    )
  }

  object IndexedView {
    implicit def index0[H, T <: HList](list : H :: T): HListView0[H, T] = {
      new HListView0[H, T](list)
    }
    implicit def indexN[H, T <: HList, Prev <: IndexedView](list : H :: T)(
      implicit indexTail: T => Prev): HListViewN[H, Prev] ={
      new HListViewN[H, Prev](list.head, indexTail(list.tail))
    }
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

  type ::[H, T <: HList] = HCons[H, T]
  val :: = HCons
  val HNil = new  HNil

}
