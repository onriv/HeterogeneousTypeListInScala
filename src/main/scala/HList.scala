/**
  * Created by river on 17/4/16.
  */
sealed trait HList {}

object HList {
  final case class HCons[H, T <: HList](head: H, tail: T) extends HList {
    def ::[T](v: T) = HCons(v, this)

    override def toString: String = s"${head} :: ${tail}"
  }

  final class HNil extends HList {
    def ::[T](v: T) = HCons(v, this)

    override def toString: String = "HNil"
  }

  type ::[H, T <: HList] = HCons[H, T]
  val :: = HCons
  val HNil = new  HNil
}
