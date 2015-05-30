package fp.property

import Prop._

trait Prop {
  def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check match {
      case Right(count1) => p.check match {
        case Right(count2) => Right(count1 + count2)
        case Left((failure, count2)) => Left((failure, count1 + count2))
      }

      case f @ Left(_) => f
    }
  }

  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}
