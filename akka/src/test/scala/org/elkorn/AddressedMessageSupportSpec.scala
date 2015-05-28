package org.elkorn

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.testkit.ImplicitSender
import akka.testkit.TestActorRef
import akka.testkit.TestKit
import akka.testkit.TestProbe
import java.util.concurrent.TimeoutException
import org.scalatest._
import scala.concurrent.Await
import scala.concurrent.duration._

class AddressedMessageSupportSpec
    extends TestKit(ActorSystem())
    with ImplicitSender
    with WordSpecLike
    with Matchers {

  import AddressedMessageSupport._
  case object TestMessage

  "An actor with AddressedMessageSupport" should {
    "send itself the inner message of an AddressedMessage" in {
      val mocks = new Mocks
      val actor = TestActorRef(new Actor with AddressedMessageSupport {
        def testReceive: Receive = {
          case Repliable(inner, _) => mocks.receiver.ref ! inner
        }

        def receive = testReceive orElse receiveAddressedMessage
      })

      actor.receive(AddressedMessage(TestMessage, testActor))

      mocks.receiver.expectMsg(TestMessage)
    }
  }

  "An addressed message" should {
    "expose a handle to its completion" in {
      val mocks = new Mocks
      val unrepliedMsg = AddressedMessage(TestMessage, mocks.receiver.ref)
      val repliedMsg = AddressedMessage(TestMessage, mocks.receiver.ref)
      val actor = TestActorRef(new Actor with AddressedMessageSupport {
        def testReceive: Receive = {
          case rep @ Repliable(inner, _) => rep.reply(Some(inner))
        }

        def receive = testReceive orElse receiveAddressedMessage
      })

      evaluating {
        Await.result(unrepliedMsg.completion, 1 second)
      } should produce[TimeoutException]

      actor.receive(repliedMsg)

      Await.result(repliedMsg.completion, 100 milliseconds) should equal(Some(repliedMsg.message))
    }
  }

  class Mocks {
    val receiver = TestProbe()
  }

}
