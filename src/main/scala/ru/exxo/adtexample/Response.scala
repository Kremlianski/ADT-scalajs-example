package ru.exxo.adtexample

sealed trait Response {
  def convert(): String
}

case object OK extends Response {
  def convert(): String = "OK"
}

case object NotEnoughMoney extends Response {
  def convert(): String = "Not enough money"
}

case object NotEnoughAsset extends Response {
  def convert(): String = "Not enough asset"
}

case object NoTransactions extends Response {
  def convert(): String = "No transactions"

}

object Response {
  implicit def ResponseToString[T <: Response](resp: T): String = resp.convert()
}


