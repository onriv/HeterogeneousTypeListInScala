/**
  * Created by river on 17/4/16.
  */

object HList {
  type ::[H, T <: HList] = HCons[H, T]
  val :: = HCons
  val HNil = new  HNil
}

sealed trait HList {
  type Expand[IfHCons <: Up, IfHNil <: Up, Up] <: Up
  type AsInitAndLast <: InitAndLastView
}

final case class HCons[H, T <: HList](head: H, tail: T) extends HList {
  type Expand[IfHCons <: Up, IfHNil <: Up, Up] = IfHCons
  type AsInitAndLast = T#Expand[
    InitAndLastViewHCons[H, T#AsInitAndLast],
    InitAndLastViewHNil[H],
    InitAndLastView
    ]
  def ::[T](v: T) = HCons(v, this)
  override def toString: String = s"${head} :: ${tail}"
}

final class HNil extends HList {
  type Expand[IfHCons <: Up, IfHNil <: Up, Up] = IfHNil
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
  type last = NextLastView#last
  def last = v.last
  def init = HCons(x, v.init) // 为何这里不能写 x :: v.init
}


