package ru.exxo.adtexample

import scala.scalajs.js
import scala.scalajs.js.annotation.ScalaJSDefined


sealed trait Message

case class Sell  (
                   name: String,
                   amount: Int,
                   price: Int
                 ) extends Message


case class Buy(
                name: String,
                amount: Int,
                price: Int
              ) extends Message

@ScalaJSDefined
abstract class MessageJs extends js.Object {
  val tipe: String
}

@ScalaJSDefined
class SellJs extends MessageJs {
  val tipe = "sell"
  var name: String = _
  var amount: Int = _
  var price: Int = _

}

@ScalaJSDefined
class BuyJs extends MessageJs {
  val tipe = "buy"
  var name: String = _
  var amount: Int = _
  var price: Int = _
}

object SellJs {
  def apply(_name: String, _amount: Int, _price: Int): SellJs =
    new SellJs {
      amount = _amount
      price = _price
      name = _name
    }
}

object BuyJs {
  def apply(_name: String, _amount: Int, _price: Int): BuyJs =
    new BuyJs {
      amount = _amount
      price = _price
      name = _name
    }

}

object MessageImplicits {

  implicit class MessageToBack(msg: MessageJs) {
    def convert(): Message =
      msg match {
        case x if x.tipe == "sell" =>
          val y = x.asInstanceOf[SellJs]
          Sell(y.name, y.amount, y.price)
        case x if x.tipe == "buy" =>
          val y = x.asInstanceOf[BuyJs]
          Buy(y.name, y.amount, y.price)
      }


  }

  implicit class MessageToFront(msg: Message) {
    def convert(): MessageJs =
      msg match {
        case Sell(x, y, z) => SellJs(x, y, z)
        case Buy(x, y, z) => BuyJs(x, y, z)
      }
  }

  implicit def messageFrontToBack(msgJs: MessageJs): Message =
    msgJs.convert()


  implicit def messageBAckToFront(msg: Message): MessageJs =
    msg.convert()


}

