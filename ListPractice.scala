package com.test

import scala.annotation.tailrec


class ListNote {

  def reverse[T](xs: List[T]): List[T] = {
    xs match {
      case Nil => xs
      case h :: tail => reverse(tail) ::: List(h)
    }
  }

  def unique[T](xs: List[T]): List[T] = {
    xs match {
      case Nil => Nil
      //case h:: tail if tail == List() => List(h)
      case h :: tail => if (unique(tail).forall(x => x != h)) h :: unique(tail) else unique(tail)
    }
  }

  @tailrec
  final def last[T](xs: List[T]): Option[T] = xs match {
    case Nil => None
    case h :: List() => Option(h)
    case _ :: tail => last(tail)
  }

  @tailrec
  final def lastNth[T](n: Int, xs: List[T]): T = {
    xs match {
      case _ :: tail if (tail.length == n) => tail.head
      case _ if (xs.length == n) => xs.head
      case _ :: tail => lastNth(n, tail)
      case _ => throw new NoSuchElementException
    }

  }

  def getLength[A](xs: List[A]): Int = xs match {
    case Nil => 0
    case h :: tail => 1 + getLength (tail)
  }

  def getLength2[A](xs: List[A]): Int = {
    @tailrec
    def getLen(k:Int,xs:List[A]):Int =(k,xs) match{
      case (n,Nil) => n
      case (n,h::tail) => getLen(n+1,tail)
    }
    getLen(0,xs)
  }

  @tailrec
  final def isPalindrome[A](xs:List[A]):Boolean = xs match {
    case Nil => true
    case head::Nil => true
    case _  if (xs.head == xs.last) => isPalindrome(xs.tail.take(xs.tail.size -1 ))
    case _ => false
  }

  @tailrec
  final def nth[A](xs: List[A],n:Int):A= n match{
    case x if x < 0  => throw new NoSuchElementException
    case 0 => xs.head
    //case h::tail if tail.length == n-1 => h
    case _=> nth(xs.tail,n-1)
  }

  /** P02: Find the last but one element of a list. */
  @tailrec
  final def penultimate[A](xs: List[A]):A = xs match {
    case Nil => throw new NoSuchElementException
    case head::Nil => throw new NoSuchElementException
    case head::x::Nil => head
    case head::tail  => penultimate(tail)
  }

  @tailrec
  final def lastEle[A](xs: List[A]):A = xs match {
    case Nil => throw new NoSuchElementException
    case head::Nil => head
    case head::tail  => lastEle(tail)
  }

  def getSumSet(xs:List[Int], n:Int):List[List[Int]]= xs match {
    case Nil => Nil
    case h::Nil  => if (h == n) List(List(h)) else Nil
    case h:: tail if h > n => getSumSet(tail,n)
    case h::tail =>  getSumSet(tail,n-h).map(x=>h::x) ::: getSumSet(tail,n) ::: (if(n ==h) List(List(h)) else Nil)
  }


  def getCoinSet(xs:List[Int], n:Int):List[List[Int]]= xs match {
    case Nil => Nil
    case h::Nil  => if (h == n) List(List(h)) else Nil
    case h:: tail if h > n => getCoinSet(tail,n)
    case h::tail =>  getCoinSet(xs,n-h).map(x=>h::x) ::: getCoinSet(tail,n) ::: (if(n ==h) List(List(h)) else Nil)
  }
  
  
  /** Checks brackets in string are balanced or not */
  def balanced(s: String): Boolean = {

    @tailrec
    def bal(xs: List[Char], stack: List[Char]): Boolean = xs match {
      case Nil => if (stack.isEmpty) true else false
      case h :: tail if (h == '(') => bal(tail, h :: stack)
      case h :: tail if (h == ')') => if (stack.isEmpty) false else bal(tail, stack.tail)
      case h :: tail => bal(tail, stack)
    }

    bal(s.toList, List[Char]())
  }
  
  /* Check there is pair of ints that add up to the target */
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    var map = scala.collection.mutable.Map[Int, Int]()
    val result = new Array[Int](2)
    nums.indices.foreach(i => {
      val value = map.get(nums(i))
      if (value.isDefined) {
        result(0) = value.get
        result(1) = i
        return result
      }
      map(target - nums(i)) = i
    })

    result

  }
  
  /* Add the the numbers in the two list */
  def addTwoNum(num1: List[Int], num2: List[Int]): List[Int] = {
    @tailrec
    def myAdd(num1: List[Int], num2: List[Int], c: Int, res: List[Int]): List[Int] = (num1, num2) match {
      case (Nil, Nil) => if (c == 0) res else res ::: List(c)
      case (Nil, n2) => myAdd(List(c), n2, 0, res)
      case (n1, Nil) => myAdd(n1, List(c), 0, res)
      case (h1 :: tail1, h2 :: tail2) => {
        val sum = h1 + h2 + c
        myAdd(tail1, tail2, sum / 10, res ::: List(sum % 10))
      }
    }

    myAdd(num1, num2, 0, List[Int]())

  }

