package com.hmrc.shoppingcart

/**
 * To define online application offers.
 * @author pghawalkar
 */
object Offers {

  /**
   * Offer for Items : Bye one and get one
   * @param noOfItems     Number of item selected
   * @param oneItemPrice  Each item price
   * @return
   */
  def buyOneGetOneOffer(noOfItems: Int, oneItemPrice: Double): Double = ((noOfItems / 2) + (noOfItems % 2)) * oneItemPrice

  /**
   * Offer for Items : 3 for the price of 2
   * @param noOfItems     Number of item selected
   * @param oneItemPrice  Each item price
   * @return
   */
  def threeForTwoOffer(noOfItems: Int, oneItemPrice: Double): Double = (2 * (noOfItems/3) + (noOfItems %3)) * oneItemPrice

  /**
   * Calculate offers based on item types: Apples, Oranges
   * @param item  item : Apple or Orange
   * @return
   */
  def offer(item: String): Int => Double = {
    val price: Double = ShoppingCart.getPrice(item)
    item toLowerCase() match {
      case "apple" => (noOfItems:Int) => buyOneGetOneOffer(noOfItems, price)
      case "orange" => (noOfItems:Int) => threeForTwoOffer(noOfItems, price)
      case _ => (noOfItems:Int) => noOfItems * price
    }
  }

}
