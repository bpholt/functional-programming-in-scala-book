package fpinscala.parallelism

import java.util.concurrent._

/* Definitions from java.util.concurrent._:

      class ExecutorService {
        def submit[A](a: Callable[A]): Future[A] = ???
      }
      trait Callable[A] { def call: A }
      trait Future[A] {
        def get: A
        def get(timeout: Long, unit: TimeUnit): A
        def cancel(evenIfRunning: Boolean): Boolean
        def isDone: Boolean
        def isCancelled: Boolean
      }
 */

sealed trait Par[A]
case class CompletedPar[A](a: A) extends Par[A]
case class UnevalPar[A](fa: ExecutorService => A) extends Par[A]

object Par {
  def unit[A](a: A): Par[A] = CompletedPar(a)

  def map2[A, B, C](pa: Par[A], pb: Par[B])
                   (f: (A, B) => C): Par[C] = {
    (pa, pb) match {
      case (CompletedPar(a), CompletedPar(b)) => unit(f(a,b))
      case (UnevalPar(fa), CompletedPar(b)) => UnevalPar(service => {
        val futureA = service.submit(new Callable[A] {
          override def call: A = fa(service)
        })

        f(futureA.get, b)
      })

      case (CompletedPar(a), UnevalPar(fb)) => UnevalPar(service => {
        val futureB = service.submit(new Callable[B] {
          override def call: B = fb(service)
        })

        f(a, futureB.get)
      })

      case (UnevalPar(fa), UnevalPar(fb)) => UnevalPar(service => {
        val futureA = service.submit(new Callable[A] {
          override def call: A = fa(service)
        })

        val futureB = service.submit(new Callable[B] {
          override def call: B = fb(service)
        })

        f(futureA.get, futureB.get)
      })
    }
  }

  def fork[A](a: => Par[A]): Par[A] = UnevalPar(s => run(s)(a))

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): A =
    a match {
      case CompletedPar(a) => a
      case UnevalPar(fa) => fa(s)
    }
}

object Tester extends App {
  import Par._
  val s = Executors.newFixedThreadPool(2)

  private val value: Par[Int] = map2(fork(unit {
    Thread.sleep(1000L)
    println("slow start")
    21
  }), fork(unit {
    println("fast start")
    21
  }))(_ + _)

  println("starting")

  println(run(s)(value))

  println("finishing")
  s.shutdown()
}
