package com.exercise

object Lists {

  //P01 Find the last element of a list.
  // The `[A]` allows us to handle lists of any type.

  def last[A](ls : Seq[A]) : A = {
    /* The standard functional approach is to recurse down the list until we hit
     the end.  Scala's pattern matching makes this easy.
     */
    def recursive[A](ls : Seq[A]): A = ls match {
      case head :: Nil => head   //Nil as an empty list, and :: as cons (pronounced)
      case _ :: tail => recursive(tail)
      case Nil => throw new NoSuchElementException
    }

    //Built in function of scala
    def bultIn[A](ls : Seq[A]): A = ls.last

    recursive(ls)
  }

  //P02  Find the last but one element of a list.

  def penultimate[A](ls : List[A]) : A = {
    def recursive[A](ls : List[A]) : A = ls match {
      case head :: _ :: Nil => head  //_ is last
      case _ :: tail => recursive(tail)  //_ is head
      case Nil |  _ => throw new NoSuchElementException   // | is or
    }

    //Built in function of Scala
    def builtIn[A](ls : List[A]): A = {
      if (ls.isEmpty) throw new NoSuchElementException
      else ls.init.last  //init = returns the list with out its last element
    }

    //Built in function using else if
    def builtIn2[A](ls : List[A]) : A = {
      if(ls.length > 1) ls(ls.length-2)
      else throw new NoSuchElementException
    }

    recursive(ls)
  }


  //P03 Find the Kth element of a list
  //By convention, the first element in the list is element 0.
  def nth[A](n : Int, ls : List[A]) : A = {
    if(n < 0 ) throw new NoSuchElementException
    def nthRecursive[A](n: Int, ls: List[A]) : A = (n, ls) match {
      case(0, head :: _) => head
      //case(n < 0, Nil) => throw new NoSuchElementException
      case(n, _ :: tail) => nthRecursive(n-1, tail)
      case(_, Nil) => throw new NoSuchElementException
    }

    def buitlIn[A](n:Int, ls : List[A]) : A = {
      if(n >= 0 & n < ls.length) ls(n)
      else throw new NoSuchElementException
    }
    nthRecursive(n, ls)
  }

  //P04 Find the number of elements of a list.
  def length[A](ls : List[A]) : Int ={
    def recursive[A](ls: List[A]) : Int = ls match {
      case Nil => 0
      case(_ :: tail) => 1 + recursive(tail)
    }

    def builtIn[A](ls : List[A]) : Int = ls.length

    recursive(ls)
  }

  //P05 Reverse a list
  def reverse[A](ls : List[A]) : List[A] = {
    def recursive[A](ls : List[A]) : List[A] = ls match {
      case Nil => Nil
      case head :: tail => reverse(tail) ::: List(head)  // ::: join or concatenate two Scala lists in one
    }

    def builtIn[A](ls : List[A]) : List[A] = ls.reverse

    recursive(ls)
  }

  // P06 Find out whether a list is a palindrome.
  def isPalindrome[A](ls : List[A]) : Boolean = {

    def usingAbove[A](ls : List[A]) : Boolean = ls == reverse(ls)

    def builtIn[A](ls : List[A]) : Boolean = ls == ls.reverse

    usingAbove(ls)
  }

  //P07 Flatten a nested list structure
  //     scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  //     res0: List[Any] = List(1, 1, 2, 3, 5, 8)

  def flatten(ls: List[Any]) : List[Any] = {
    def recursive(ls: List[Any]) : List[Any] = ls match {
      case(head : List[_]) :: tail => println(ls); recursive(head) ::: recursive(tail)
      case head :: tail =>  head :: recursive(tail)
      case Nil => Nil
    }

    recursive(ls)
  }

  //P08 Eliminate consecutive duplicates of list elements.
  def compress[A](ls:List[A]) : List[A] = {
    def recursive[A](ls : List[A]) : List[A] = ls match {
      case Nil => Nil
      case head :: tail => {
        if (tail.isEmpty) List(head)
        else if(head == tail.head) recursive(tail)
        else head :: recursive(tail)
      }
    }

    recursive(ls)
  }
}
