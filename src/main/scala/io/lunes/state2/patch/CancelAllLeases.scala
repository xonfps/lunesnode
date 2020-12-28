package io.lunes.state2.patch

import io.lunes.state2.reader.SnapshotStateReader
import io.lunes.state2.{Diff, LeaseInfo, Portfolio}

/** Cancel All Leases object.*/
object CancelAllLeases {
  /** The Application method for the Cancel All Leases object.
    * @param s The Snapshot State Reader.
    * @return Returns a Diff object.
    */
  def apply(s: SnapshotStateReader): Diff = {

    def invertLeaseInfo(l: LeaseInfo): LeaseInfo = LeaseInfo(-l.leaseIn, -l.leaseOut)

    val portfolioUpd = s.accountPortfolios
      .collect { case (acc, pf) if pf.leaseInfo != LeaseInfo.empty =>
        acc -> Portfolio(0, invertLeaseInfo(pf.leaseInfo), Map.empty)
      }

    Diff(transactions = Map.empty,
      portfolios = portfolioUpd,
      issuedAssets = Map.empty,
      aliases = Map.empty,
      orderFills = Map.empty,
      leaseState = s.activeLeases().map(_ -> false).toMap)
  }

}
