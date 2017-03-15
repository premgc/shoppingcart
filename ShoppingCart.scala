package com.hmrc.shoppingcart

import scala.math.BigDecimal.RoundingMode

/**
 * Represent Shopping Cart
 * @author pghawalkar
 */
object ShoppingCart {

  // Each Item price.
  private val itemsPriceList: Map[String, Double] = Map.apply(Tuple2("apple", scala.runtime.BoxesRunTime.boxToDouble(0.60)), Tuple2("orange", scala.runtime.BoxesRunTime.boxToDouble(0.25)))

  /**
   * To perform checkout operation with list of selected items
   * @param items A list of items
   * @return      Total cost
   */
  def checkout(items: Array[String]): Double = {
   val total = items.flatMap(price).sum
    BigDecimal.apply(total).setScale(2, RoundingMode.HALF_EVEN).toDouble
  }

  /**
   * To perfrom checkout operation with list of selected items and provided offers
   * @param items
   * @return
   */

  def checkoutWithOffers(items: Array[String]): Double = {
    val noOfApples: Int = refArrayOps(items).count((p: _root_.scala.Predef.String) => p.equalsIgnoreCase("apple"))
    val noOfOranges: Int = refArrayOps(items).count((p: _root_.scala.Predef.String) => p.equalsIgnoreCase("orange"))

    // To calculate total items price based on offers
    val totalPrice: Double = Offers.offer("apple")(noOfApples).+(Offers.offer("orange")(noOfOranges))

    // To round total price to half even
     BigDecimal.apply(totalPrice).setScale(2, RoundingMode.HALF_EVEN).toDouble
  }

  def getPrice(item: String): Double = {
     itemsPriceList.getOrElse(item.toLowerCase,0)
  }

  def price(item: String): Option[Double] =  itemsPriceList.get(item.toLowerCase)

}
