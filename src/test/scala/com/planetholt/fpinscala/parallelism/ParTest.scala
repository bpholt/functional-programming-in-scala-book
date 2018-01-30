package com.planetholt.fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Executors}

import org.scalatest.{AsyncFlatSpec, Matchers}

import scala.concurrent.Future

class ParTest extends AsyncFlatSpec with Matchers {

  val executorService: ExecutorService = Executors.newWorkStealingPool()

  import Par._

  behavior of "max"

  it should "find the maximum" in Future {
    val max = Par.max(IndexedSeq(4, 6, 42, -1)).run(executorService)

    max should be(Option(42))
  }

  it should "find the maximum when all numbers are negative" in Future {
    val max = Par.max(IndexedSeq(-42, -1, -4, -2)).run(executorService)

    max should be(Option(-1))
  }

  it should "be none for empty list" in Future {
    val max = Par.max(IndexedSeq.empty[Int]).run(executorService)

    max should be(None)
  }

  behavior of "map3"

  it should "add three numbers" in Future {
    val output = map3(unit(1), unit(2), unit(3))(_ + _ + _).run(executorService)

    output should be(6)
  }

  behavior of "laws"

  it should "equal" in Future {
    val output = Par.equal(executorService)(unit(1).map(_ + 1), unit(2))

    output should be(true)
  }

}
