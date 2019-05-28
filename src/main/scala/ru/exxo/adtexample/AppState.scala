package ru.exxo.adtexample

import MessageImplicits._

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

case class Asset(
                  name: String,
                  amount: Int
                )

case class AppState(
                     cash: Int = 10000,
                     assets: Map[String, Asset] = Map.empty,
                     transactions: List[Message] = List.empty
                   )
@JSExportTopLevel("AppState")
object AppState {

  private var state = AppState()

  @JSExport
  def process(msg: MessageJs): String = processMessage(msg)

  private def processMessage(msg: Message): Response = msg match {

    case Buy (x, y, z) if y * z <= state.cash =>
      state = state.copy(
        cash = state.cash - y * z,
        assets = state.assets + (x -> state.assets.get(x).map(a => a.copy(amount = a.amount + y)).getOrElse(Asset(x, y))),
        transactions =  Buy (x, y, z) :: state.transactions
      )
      OK
    case Buy (_, _, _) => NotEnoughMoney
    case Sell (x, y, z) if  y <= state.assets.get(x).map(_.amount).getOrElse(0) =>

      val asset: (String, Option[Asset]) = x -> state.assets.get(x).map(a => a.copy(amount = a.amount - y))
      state = state.copy(
        cash = state.cash + y * z,
        assets = asset match {
          case (_, None )=> state.assets
          case (_, Some(v)) => v match {
            case Asset(n, 0) => state.assets - n
            case Asset(n, a) => state.assets + (n -> Asset(n, a))
          }
        },
        transactions =   Sell(x, y, z) :: state.transactions
      )
      OK
    case Sell (_, _, _)  => NotEnoughAsset

  }

  @JSExport
  def countTransactions(): Int = state.transactions.size

  @JSExport
  def countAssets(): Int = state.assets.size

  @JSExport
  def assetAmount(name: String): Int = state.assets.get(name).fold(0)(_.amount)

  @JSExport
  def cash(): Int = state.cash

  @JSExport
  def undo(): String = {
    val transaction: Option[Message] = state.transactions.headOption

    val result: Response = transaction.fold ( NoTransactions.asInstanceOf[Response]) {

      case Buy(x, y, z) => processMessage(Sell(x, y, z))
      case Sell(x, y, z) => processMessage(Buy(x, y, z))
    }

    if(result == OK) state = state.copy(transactions = state.transactions.tail.tail)

    result
  }

}

