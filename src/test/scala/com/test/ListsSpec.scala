package com.test

import org.scalatest.FreeSpec
import com.exercise.Lists._

class ListsSpec extends FreeSpec{

  //P01
  "last()" - {
    "should return last elements of the multiple elements list" in {
      assert(last(Seq(1,2,3,4)) == 4)
    }
    "should throw exception if seq is empty" in {
      assertThrows[NoSuchElementException]{
        last(Seq())
      }
    }
  }


  //P02
  "penultimate()" - {
    "should return second element if the multiple elements in list" in {
      assert(penultimate(List(1,2,3,4,5)) == 4)
    }
    "should throw exception if the list is empty" in {
      assertThrows[NoSuchElementException]{
        penultimate(List())
      }
    }
    "should throw exceprion when there is just one element in the list" in {
      assertThrows[NoSuchElementException]{
        penultimate(List(1))
      }
    }
  }

  //P03
  "nth()" - {
    "should return first element if n is 0" in {
      assert(nth(0, List(1,2,3,4)) == 1)
    }
    "should return nth element from the list" in {
      assert(nth(3, List(1,2,3,4,5)) == 4)
    }
    "should throw when n is negative" in {
      assertThrows[NoSuchElementException]{
        nth(-1, List(1,2,3))
      }
    }
    "should throw when list is shorter than n-1" in {
      assertThrows[NoSuchElementException] {
        nth(3, List(1, 2, 3))
      }
    }
  }

  //P04
  "length()" - {
    "should return the length of the list" in {
      assert(length(List(1,2,3,4,5,6)) == 6)
    }
    "should throw if the list is empty" in {
      assertThrows[NoSuchElementException] {
        length(List())
      }
    }
  }

  //P05
  "reverse()" - {
    "should reverse the list if the length is > 0" in {
      assert(reverse(List(1,2,3,4)) == List(4,3,2,1))
    }
    "should return empty if the list is empty" in {
      assert(reverse(List()) == List())
    }
  }

  //P06
  "isPalindrome()" - {
    "should return true if the list id isPalindrome" in {
      assert(isPalindrome(List(1,2,3,2,1)) == true)
    }
    "is not isPalindrome" in {
      assert(isPalindrome(List(1,2,3,4,5)) == false)
    }
  }

  //P07
  "flatten()" - {
    "should flatten the list " in {
      assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
    }
  }
}
