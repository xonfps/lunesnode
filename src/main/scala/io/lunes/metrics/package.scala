package io.lunes

import kamon.metric.instrument.Histogram

/** Package object for Metrics. */
package object metrics {

  /** Histogram Extension.
    * @param h Input a [[kamon.metric.instrument.Histogram]].
    */
  final implicit class HistogramExt(val h: Histogram) extends AnyVal {
    def safeRecord(value: Long): Unit = h.record(Math.max(value, 0))
  }
}
