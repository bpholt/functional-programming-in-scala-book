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

  def asyncF[A, B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))
/*
    a =>
    map(lazyUnit(a))(f)
*/

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((a, _) => a.sorted)

  def map[A,B](pa: Par[A])
              (f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))
//    ps.foldRight(lazyUnit(List.empty[A])) { (parA, pas) =>
//      map2(parA, pas)( { (a, l) =>
//        a :: l
//      })
//    }

  def parMap[A, B](ps: List[A])
                  (f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])
                  (f: A => Boolean): Par[List[A]] = {
    val parF: A => Par[Boolean] = asyncF(f)

    // sequence version
//    val step1: List[Par[(A, Boolean)]] = as.map { a =>
//      val parBoolean = parF(a)
//      map2(unit(a), parBoolean)(_ -> _)
//    }
//
//    val step2: Par[List[(A, Boolean)]] = sequence(step1)
//
//    val output: Par[List[A]] = map(step2)(_.filter(_._2).map(_._1))
//    output

    // raw foldRight version
    as.foldRight(lazyUnit(List.empty[A])) { (a: A, parAs: Par[List[A]]) =>
      map2(parF(a), parAs) { (b, as) =>
        if (b) a :: as else as
      }
    }
  }

}

object Tester extends App {
  import Par._
  val s = Executors.newFixedThreadPool(20)

  private val slowIncrement: Int => Par[Int] = asyncF { i =>
    println(s"asyncF start $i on ${Thread.currentThread().getName}")
    Thread.sleep(1000L)
    println(s"asyncF end $i")
    i + 1
  }

  def doFirstExample(): Unit = {
    val value: Par[Int] = map2(slowIncrement(20), fork(unit {
      println(s"fast start on ${Thread.currentThread().getName}")
      21
    }))(_ + _)

    println(s"starting on ${Thread.currentThread().getName}")
    println(run(s)(value))
    println("finishing")
  }

  def doSequence(): Unit = {
    println("starting sequence")
    val parInts: List[Par[Int]] = (0 until 10).map(slowIncrement).toList
    println("we have a list of Par[Int]")
    val parListInt: Par[List[Int]] = sequence(parInts)
    println("we have a Par[List[Int]]")
    val ints: List[Int] = run(s)(parListInt)
    println(ints)
    println("finished sequence")
  }

  def doParFilter(): Unit = {
    println("starting parFilter")
    val ints: List[Int] = (0 until 10).toList

    val slowF: Int => Boolean = i => {
      println(s"starting filter $i on ${Thread.currentThread().getName}")
      Thread.sleep(1000L)
      println(s"finishing filter $i")
      i % 2 == 0
    }

    println(run(s)(parFilter(ints)(slowF)))
    println("finished parFilter")
  }


//  run(s)(map2(slowIncrement(1), slowIncrement(2))(_ + _))

//  doFirstExample()

//  doSequence()

  doParFilter()

  s.shutdown()
}
