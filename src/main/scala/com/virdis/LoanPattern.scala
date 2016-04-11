package com.virdis

/**
  * Created by User: sandeep - Project: ddl-conc.
  */

import util.control.Exception._

trait LoanPattern {

  type Closeable = { def close() }

  def using[R <: Closeable, A](resource: R)(f: R => A): A = {
    try {
      f(resource)
    } finally {
      ignoring(classOf[Throwable]) apply {
        resource.close()
      }
    }
  }

}

object resourceManager extends LoanPattern
