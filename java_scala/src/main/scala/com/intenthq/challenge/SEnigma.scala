package com.intenthq.challenge

object SEnigma {

  // We have a system to transfer information from one place to another. This system
  // involves transferring only list of digits greater than 0 (1-9). In order to decipher
  // the message encoded in the list you need to have a dictionary that will allow
  // you to do it following a set of rules:
  //    > Sample incoming message: (​1,2,3,7,3,2,3,7,2,3,4,8,9,7,8)
  //    > Sample dictionary (​23->‘N’,234->‘ ’,89->‘H’,78->‘Q’,37 ->‘A’)
  //  - Iterating from left to right, we try to match sublists to entries of the map.
  //    A sublist is a sequence of one or more contiguous entries in the original list,
  //    eg. the sublist (1, 2) would match an entry with key 12, while the sublist (3, 2, 3)
  //    would match an entry with key 323.
  //  - Whenever a sublist matches an entry of the map, it’s replaced by the entry value.
  //    When that happens, the sublist is consumed, meaning that its elements can’t be used
  //    for another match. The elements of the mapping however, can be used as many times as needed.
  //  - If there are two possible sublist matches, starting at the same point, the longest one
  //    has priority, eg 234 would have priority over 23.
  //  - If a digit does not belong to any matching sublist, it’s output as is.
  //
  // Following the above rules, the message would be: “1N73N7 HQ”
  // Check the tests for some other (simpler) examples.

  /**
   * Note on solution
   *
   * This solution uses the assumption that dictionary entry values will never be numeric, as is the case in the
   * description and the provided test cases. If translated values were to include numeric characters, the solution
   * would need to take a different approach.
   *
   */

  def decipher(map: Map[Int, Char])(message: List[Int]): String = {

    def orderByStringDescThenIntAsc = {
      Ordering.Tuple2(Ordering.Int.reverse, Ordering.String)
    }

    def sortByKeyLengthDescThenValueAsc(seqToSort: Seq[(String, Char)]) = {
      seqToSort.sortBy(r => (r._1.length, r._1))(orderByStringDescThenIntAsc)
    }

    val sortedDictionary = sortByKeyLengthDescThenValueAsc(map.toSeq.map(x => (x._1.toString, x._2)))

    val messageAsString = message.foldLeft("")((s, i) => s.concat(i.toString))

    sortedDictionary.foldLeft(messageAsString)((m, x) => m.replaceAll(x._1, x._2.toString))
  }

}