package com.test.future

import org.scalatest.{FunSuite, Matchers}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class FibTest extends FunSuite with Matchers {

  def fibClassic(n: BigInt): BigInt = if (n == 0) 0 else n + fibClassic(n - 1)

  /**
    * Actually, it creates n+1 of short-lived independent futures, and only return the value from last one
    * Future(n, 0, 1)
    * Future(n-1, 1, 1)
    * Future(n-2, 1, 2)
    * ...
    * Future(0, finalResult - n, finalResult )
    * Each future only needs a single level stack, virtually equivalent to tail recursion.
    *
    */

  def fibFuture(n: BigInt): BigInt = {
    def fib(n: BigInt, a: BigInt, b: BigInt): Future[BigInt] = {
      if (n == 0) Future(b) else fib(n - 1, b, a + b)
    }

    Await.result(fib(n, 0, 1), 3.seconds)
  }

  def fibTailRec(n: BigInt): BigInt = {
    @tailrec
    def fib(n: BigInt, a: BigInt, b: BigInt): BigInt = if (n == 0) b else fib(n - 1, b, a + b)

    fib(n, 0, 1)
  }

 test("mutual recursion -- fails due to stack overflow"){
   def F(n:Int):Int =  if (n == 0) 1 else n - M(F(n-1))
   def M(n:Int):Int = if (n == 0) 0 else n - F(M(n-1))

   println(F(100000))
 }

  test("mutual recursion future -- fails due to stack overflow "){
    def F(n:Int): Future[Int] ={
      if (n == 0) Future(1)
      else for{ fn_1 <- F(n-1)
                x <- M(fn_1)
              } yield n - x
    }
    def M(n:Int):Future[Int] = if (n == 0) Future(0) else for{ mn_1 <- M(n-1); x <- F(mn_1)} yield n - x

    F(100000).map(x=> println(x))
  }

  test("fibClassis -- fail due to the stack overflow") {
    val fib = fibClassic(100000)
  }

  test("fibTailRec -- works") {
    val fib = fibTailRec(100000)
    println(fib)
  }


  test("fibFuture -- works") {
    val fib = fibFuture(100000)

    println(fib)
  }

}
