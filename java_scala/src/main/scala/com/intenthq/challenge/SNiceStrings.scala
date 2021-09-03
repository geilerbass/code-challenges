package com.intenthq.challenge

import scala.util.matching.Regex

object SNiceStrings {

// From http://adventofcode.com/day/5
//  --- Day 5: Doesn't He Have Intern-Elves For This? ---
//
//  Santa needs help figuring out which strings in his text file are naughty or nice.
//
//    A nice string is one with all of the following properties:
//
//    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
//  It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
//    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
//    For example:
//
//    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
//  aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
//    jchzalrnumimnmhp is naughty because it has no double letter.
//    haegwjzuvuyypxyu is naughty because it contains the string xy.
//    dvszwmarrgswjxmb is naughty because it contains only one vowel.
//    How many strings are nice?

  def nice(xs: List[String]): Int = {
    val booleans = xs.map(isANiceString)
    booleans.count(_ == true)
  }

  private def isANiceString: String => Boolean = { s: String =>
    def hasNVowels(n: Int) = { s: String =>
      val r : Regex = "[aeiou]".r
      val size = r.findAllMatchIn(s.toLowerCase).size
      size >= n
    }

    def hasNRepeated(n: Int) = { s: String =>
      s.toCharArray.sliding(n).exists {
        case Array(x, y) => x == y
      }
    }

    def doesNotContain(strings: List[String]) = { s: String =>
      !strings.map(_.r.findFirstMatchIn(s)).exists(_.isDefined)
    }

    val validations = List(hasNVowels(3), hasNRepeated(2), doesNotContain(List("ab", "cd", "pq", "xy")))
    validations.foldLeft(true)((x, y) => y.apply(s) && x)
  }
}
