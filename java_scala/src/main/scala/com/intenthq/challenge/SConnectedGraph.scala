package com.intenthq.challenge

case class Node(value: Int, edges: List[Node] = List.empty)

object SConnectedGraph {

  // Find if two nodes in a directed graph are connected.
  // Based on http://www.codewars.com/kata/53897d3187c26d42ac00040d
  // For example:
  // a -+-> b -> c -> e
  //    |
  //    +-> d
  // run(a, a) == true
  // run(a, b) == true
  // run(a, c) == true
  // run(b, d) == false

  /**
   * Note on solution
   *
   * The Codewars article mentioned that is referenced, specifies both acyclic and cyclic graphs.
   *
   * However, as the provided unit tests do not specify the cyclic scenario, my solution omits support for this. The
   * immutable 'Node' case class does not allow creation of a cyclic graph, so if this scenario were required, that
   * class would need to be amended to make the edges field a 'var'.
   *
   * To avoid the function looping endlessly for cyclic graphs, it would include a stack for keeping track of which
   * nodes are on the current traversal path.
   */

  def run(source: Node, target: Node): Boolean = {

    def isConnected(target: Node, startingNode: Node): Boolean = {
      if (startingNode == target) {
        true
      } else {
        startingNode.edges.exists(isConnected(target, _))
      }
    }

    isConnected(target, source)
  }
}
