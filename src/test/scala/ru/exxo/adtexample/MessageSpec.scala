package ru.exxo.adtexample

import org.scalatest._

import MessageImplicits._
//import Response._


class MessageSpec extends FreeSpec {

  val message = Buy("b", 10, 100)

  val Ok: OK.type = OK

  def testAutoConversion(str: Response): String = str

  def testAutoConversion1(str: String): String = str

  " blah- blah -blah " in {


    assert(message.convert().convert == message)

  }

  " blah- blah " in {
    assert(Ok.convert() == "OK")
    assert(NotEnoughMoney.convert() == "Not enough money")
    assert(NotEnoughAsset.convert() == "Not enough asset")
  }

  " blah " in {
    assert(testAutoConversion(Ok) == "OK")
    assert(testAutoConversion1(Ok) == "OK")
    assert(testAutoConversion(NotEnoughMoney) == "Not enough money")
    assert(testAutoConversion1(NotEnoughMoney) == "Not enough money")
    assert(testAutoConversion(NotEnoughAsset) == "Not enough asset")
    assert(testAutoConversion1(NotEnoughAsset) == "Not enough asset")
  }

  "lalala" in {

    val sell = SellJs("A", 10, 10)

    assert(AppState.process(sell) == "Not enough asset")

    assert(AppState.cash() == 10000)

    assert(AppState.assetAmount("A") == 0)

    assert(AppState.countAssets() == 0)

    assert(AppState.countTransactions() == 0)

    assert(AppState.undo() == "No transactions")

    val buy = BuyJs("A", 1000, 100)

    assert(AppState.process(buy) == "Not enough money")

    assert(AppState.undo() == "No transactions")

    val buy1 = BuyJs("A", 100, 10)
    val buy2 = BuyJs("B", 100, 5)
    val sell1 = SellJs("A", 10, 10)

    assert(AppState.process(buy1) == "OK")
    assert(AppState.process(buy2) == "OK")
    assert(AppState.process(sell1) == "OK")

    assert(AppState.assetAmount("A") == 90)
    assert(AppState.cash() == 8600)

    assert(AppState.countTransactions() == 3)


    assert(AppState.undo() == "OK")

    assert(AppState.countTransactions() == 2)

    assert(AppState.assetAmount("A") == 100)
    assert(AppState.cash() == 8500)


    assert(AppState.undo() == "OK")

    assert(AppState.assetAmount("A") == 100)
    assert(AppState.assetAmount("B") == 0)
    assert(AppState.cash() == 9000)

    assert(AppState.countTransactions() == 1)


  }

}
