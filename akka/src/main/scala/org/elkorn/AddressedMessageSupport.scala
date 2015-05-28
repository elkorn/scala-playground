package org.elkorn

import akka.actor.{ Actor, ActorRef }

import scala.concurrent.{ Future, Promise }

object AddressedMessageSupport {
  case class AddressedMessage[T](val message: T, val replyTo: ActorRef) {

    private[AddressedMessageSupport] val completionPromise: Promise[Any] = Promise[Any]

    def completion: Future[Any] = completionPromise.future

    def reply(rep: Any)(implicit sender: ActorRef) = {
      replyTo.tell(rep, sender)
      completionPromise.success(rep)
    }
  }

  case class Repliable[T](message: T, response: Response) {
    def reply(rep: Any) = response.reply(rep)
    def completion = response.completion
  }

  @SerialVersionUID(123L)
  private[AddressedMessageSupport] class Response() extends Serializable {
    private var sender: Option[ActorRef] = None
    private var promise: Option[Promise[Any]] = None

    private def requireIsCorrect() =
      List(sender, promise).map(x => require(x.isDefined))

    def reply(rep: Any): Unit = {
      requireIsCorrect()
      sender.map(_ ! rep)
      promise.map(_.success(rep))
    }

    def completion: Future[Any] = {
      requireIsCorrect()
      promise.get.future
    }

    private[AddressedMessageSupport] def withSender(newSender: ActorRef): Response = {
      if (sender.isDefined) throw new RuntimeException("Sender already defined.")
      sender = Some(newSender)
      this
    }

    private[AddressedMessageSupport] def withPromise(newPromise: Promise[Any]): Response = {
      if (promise.isDefined) throw new RuntimeException("Promise already defined.")
      promise = Some(newPromise)
      this
    }

    override def toString() =
      s"Response($sender, $promise)"
  }

  private def repliable[T](m: AddressedMessage[T]) =
    Repliable(
      m.message,
      new Response()
        .withSender(m.replyTo)
        .withPromise(m.completionPromise)
    )
}

trait AddressedMessageSupport {
  _: Actor =>

  import AddressedMessageSupport._

  def receiveAddressedMessage: Actor.Receive = {
    case msg @ AddressedMessage(message, replyTo) => self.tell(repliable(msg), replyTo)
  }
}
