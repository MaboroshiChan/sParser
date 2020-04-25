import Prop.{FailedCase, SuccessCount}


object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p:Prop): Prop = ???
}
