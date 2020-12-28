package io.lunes.state2

import cats._
import cats.kernel.instances.map._
import cats.Monoid
import scorex.block.Block.Fraction

/** Case Class for Portfolio.
  * @param balance The Portfolio balance.
  * @param leaseInfo The Lease Information for the Portfolio object.
  * @param assets A Map for Asset ID into Long.
  * @todo verificar a necessidade de case class
  */
case class Portfolio(balance: Long, leaseInfo: LeaseInfo, assets: Map[ByteStr, Long]) {
  lazy val effectiveBalance: Long = safeSum(balance, leaseInfo.leaseIn) - leaseInfo.leaseOut
  lazy val spendableBalance: Long = balance - leaseInfo.leaseOut

  lazy val isEmpty: Boolean = this == Portfolio.portfolioMonoid.empty
}

/** The Portfolio Companion Object.*/
object Portfolio {

  implicit val longSemigroup: Semigroup[Long] = (x: Long, y: Long) => safeSum(x, y)

  implicit val portfolioMonoid = new Monoid[Portfolio] {
    override val empty: Portfolio = Portfolio(0L, Monoid[LeaseInfo].empty, Map.empty)

    override def combine(older: Portfolio, newer: Portfolio): Portfolio
    = Portfolio(
      balance = safeSum(older.balance, newer.balance),
      leaseInfo = Monoid.combine(older.leaseInfo, newer.leaseInfo),
      assets = Monoid.combine(older.assets, newer.assets))
  }

  /** implicit class for Portfolio Extension.
    * @param self The reference for the Portfolio itself.
    */
  implicit class PortfolioExt(self: Portfolio) {
    /** Gets a pessimistic version of a Portfolio.
      * @return Returns a Portfolio with pessimistic estimates.
      */
    def pessimistic: Portfolio = Portfolio(
      balance = Math.min(self.balance, 0),
      leaseInfo = LeaseInfo(
        leaseIn = 0,
        leaseOut = Math.max(self.leaseInfo.leaseOut, 0)
      ),
      assets = self.assets.filter { case (_, v) => v < 0 }
    )

    /** Gets a Multiplied version of the Portfolio given a Fraction.
      * @param f
      * @return
      */
    def multiply(f: Fraction): Portfolio =
      Portfolio(f(self.balance), LeaseInfo.empty, self.assets.mapValues(f.apply))

    /** Gets a operated version of the Portfolio given an other Portfolio.
      * @param other The portfolio of which to difference.
      * @return Returns the operated Portfolio.
      */
    def minus(other: Portfolio): Portfolio =
      Portfolio(self.balance - other.balance, LeaseInfo.empty,
        Monoid.combine(self.assets, other.assets.mapValues(-_)))
  }

}
