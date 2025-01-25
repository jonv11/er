package org.example

import scala.collection.mutable

/**
 * EdgeBetweennessCentrality calculates the edge betweenness centrality for a graph.
 */
object EdgeBetweennessCentrality {

  /**
   * Represents an edge in the graph.
   *
   * @param src    Source vertex ID.
   * @param dst    Destination vertex ID.
   * @param weight Weight of the edge.
   */
  case class Edge(src: Int, dst: Int, weight: Double)

  /**
   * Represents a graph with a set of vertices and edges.
   *
   * @param vertices Set of vertex IDs.
   * @param edges    Sequence of edges in the graph.
   */
  case class Graph(vertices: Set[Int], edges: Seq[Edge])

  /**
   * Calculates the shortest paths from a given source vertex to all other vertices in the graph.
   *
   * @param graph  The graph.
   * @param source The source vertex ID.
   * @return A map where the key is the vertex ID and the value is a sequence of predecessor vertices.
   */
  def calculateShortestPaths(graph: Graph, source: Int): Map[Int, Seq[Int]] = {
    val distances = mutable.Map[Int, Double]()
    val predecessors = mutable.Map[Int, Seq[Int]]()

    graph.vertices.foreach { v =>
      distances(v) = Double.PositiveInfinity
      predecessors(v) = Seq()
    }

    distances(source) = 0.0
    val queue = mutable.PriorityQueue[(Double, Int)]()(Ordering.by(-_._1))
    queue.enqueue((0.0, source))

    while (queue.nonEmpty) {
      val (currentDist, currentVertex) = queue.dequeue()

      if (currentDist <= distances(currentVertex)) {
        for (Edge(_, neighbor, weight) <- graph.edges if graph.edges.contains(Edge(currentVertex, neighbor, weight))) {
          val distance = currentDist + 1 / weight

          if (distance < distances(neighbor)) {
            distances(neighbor) = distance
            predecessors(neighbor) = Seq(currentVertex)
            queue.enqueue((distance, neighbor))
          } else if (distance == distances(neighbor)) {
            predecessors(neighbor) :+= currentVertex
          }
        }
      }
    }

    predecessors.toMap
  }

  /**
   * Accumulates the edge betweenness centrality for all edges in the graph.
   *
   * @param graph The graph.
   * @return A map where the key is a tuple of source and destination vertex IDs, and the value is the edge betweenness centrality.
   */
  def accumulateEdgeBetweenness(graph: Graph): Map[(Int, Int), Double] = {
    val edgeBetweenness = mutable.Map[(Int, Int), Double]().withDefaultValue(0.0)

    for (source <- graph.vertices) {
      val shortestPaths = calculateShortestPaths(graph, source)
      val dependencies = mutable.Map[Int, Double]().withDefaultValue(0.0)
      var nodes = graph.vertices.toSeq.sortBy(n => shortestPaths(n).size)

      while (nodes.nonEmpty) {
        val w = nodes.last
        nodes = nodes.init // Removes the last element

        for (v <- shortestPaths(w)) {
          val ratio = (1.0 + dependencies(w)) / shortestPaths(w).size
          dependencies(v) += ratio
          val edge = if (v < w) (v, w) else (w, v)
          edgeBetweenness(edge) += ratio
        }
      }
    }

    edgeBetweenness.toMap
  }

  /**
   * Main method to run the Edge Betweenness Centrality calculation.
   *
   * @param args Command-line arguments (not used).
   */
  def main(args: Array[String]): Unit = {


    // https://amirieb.github.io/COMP5800_S20/files/lec-1-1.pdf
    // https://amirieb.github.io/COMP5800_S20/files/lec-1-2.pdf
    // https://amirieb.github.io/COMP5800_S20/files/lec-1-3.pdf
    // https://amirieb.github.io/COMP5800_S20/files/lec-1-4.pdf

    // https://amirieb.github.io/COMP5800_S20/files/lec-2-1.pdf
    // https://amirieb.github.io/COMP5800_S20/files/lec-2-2.pdf

    // https://amirieb.github.io/COMP5800_S20/files/lec-3-1.pdf
    // https://amirieb.github.io/COMP5800_S20/files/lec-3-2.pdf

    // https://amirieb.github.io/COMP5800_S20/files/lec-4-1.pdf
    // ...


    process(Graph(Set(1, 2, 3, 4, 5, 6), Seq(
      Edge(1, 2, 0.1), Edge(1, 3, 0.4), Edge(2, 4, 0.5),
      Edge(3, 4, 0.7), Edge(4, 5, 0.8), Edge(5, 6, 0.2)
    )), "Original Test Sample", "original_test_sample.png")

    process(Graph(Set(1, 2, 3, 4, 5, 6, 7), Seq(
      Edge(1, 2, 0.2), Edge(1, 3, 0.3), Edge(2, 4, 0.6),
      Edge(3, 4, 0.5), Edge(4, 5, 0.4), Edge(5, 6, 0.7),
      Edge(6, 7, 0.8), Edge(5, 7, 0.9)
    )), "Test Sample 1", "test_sample_1.png")

    process(Graph(Set(1, 2, 3, 4, 5, 6, 7, 8), Seq(
      Edge(1, 2, 0.1), Edge(1, 3, 0.3), Edge(2, 4, 0.4),
      Edge(3, 4, 0.6), Edge(4, 5, 0.5), Edge(5, 6, 0.7),
      Edge(6, 7, 0.2), Edge(7, 8, 0.8), Edge(3, 6, 0.9),
      Edge(2, 8, 0.4)
    )), "Test Sample 2", "test_sample_2.png")

    process(Graph(Set(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), Seq(
      Edge(1, 2, 0.2), Edge(1, 3, 0.5), Edge(2, 4, 0.3),
      Edge(3, 4, 0.4), Edge(4, 5, 0.5), Edge(5, 6, 0.6),
      Edge(6, 7, 0.2), Edge(7, 8, 0.1), Edge(8, 9, 0.8),
      Edge(9, 10, 0.3), Edge(3, 9, 0.7), Edge(2, 10, 0.4)
    )), "Test Sample 3", "test_sample_3.png")


    process(Graph(Set(1, 2, 3, 4, 5), Seq(
      Edge(1, 2, 0.2), Edge(1, 3, 0.5), Edge(1, 4, 0.3),
      Edge(1, 5, 0.4), Edge(2, 3, 0.5), Edge(2, 4, 0.6),
      Edge(2, 5, 0.2), Edge(3, 4, 0.1), Edge(3, 5, 0.8),
      Edge(4, 5, 0.3)
    )), "Test Sample 4", "test_sample_4.png")

/*
    Edge Betweenness Centrality:
      Edge (5, 6): 5.0 [0.2] => 25.0
    Edge (3, 4): 1.0 [0.7] => 1.4285714285714286
    Edge (1, 2): 2.0 [0.1] => 20.0
    Edge (4, 5): 8.0 [0.8] => 10.0
    Edge (2, 4): 2.0 [0.5] => 4.0
    Edge (1, 3): 1.0 [0.4] => 2.5
    Edge Betweenness Centrality:
      Edge (5, 6): 5.0 [0.7] => 7.142857142857143
    Edge (3, 4): 1.5 [0.5] => 3.0
    Edge (1, 2): 1.5 [0.2] => 7.5
    Edge (4, 5): 12.0 [0.4] => 30.0
    Edge (6, 7): 1.0 [0.8] => 1.25
    Edge (2, 4): 1.5 [0.6] => 2.5
    Edge (1, 3): 1.5 [0.3] => 5.0
    Edge (5, 7): 5.0 [0.9] => 5.555555555555555
    Edge Betweenness Centrality:
      Edge (5, 6): 8.0 [0.7] => 11.428571428571429
    Edge (3, 4): 1.0 [0.6] => 1.6666666666666667
    Edge (7, 8): 5.0 [0.8] => 6.25
    Edge (1, 2): 3.0 [0.1] => 30.0
    Edge (4, 5): 9.0 [0.5] => 18.0
    Edge (6, 7): 10.0 [0.2] => 50.0
    Edge (2, 4): 2.0 [0.4] => 5.0
    Edge (2, 8): 2.0 [0.4] => 5.0
    Edge (3, 6): 5.0 [0.9] => 5.555555555555555
    Edge (1, 3): 1.0 [0.3] => 3.3333333333333335
    Edge Betweenness Centrality:
      Edge (5, 6): 15.0 [0.6] => 25.0
    Edge (3, 4): 1.0 [0.4] => 2.5
    Edge (7, 8): 7.0 [0.1] => 70.0
    Edge (1, 2): 2.0 [0.2] => 10.0
    Edge (4, 5): 16.0 [0.5] => 32.0
    Edge (6, 7): 12.0 [0.2] => 60.0
    Edge (8, 9): 6.0 [0.8] => 7.5
    Edge (9, 10): 7.0 [0.3] => 23.333333333333336
    Edge (3, 9): 2.0 [0.7] => 2.857142857142857
    Edge (1, 3): 1.0 [0.5] => 2.0
    Edge (2, 4): 2.0 [0.3] => 6.666666666666667
    Edge (2, 10): 2.0 [0.4] => 5.0
*/

  }

  def process(graph: Graph): Unit = {
    val edgeBetweenness = accumulateEdgeBetweenness(graph)

    println("Edge Betweenness Centrality:")
    edgeBetweenness.foreach { case ((src, dst), betweenness) =>
      val edge = graph.edges.filter(e => e.src == src && e.dst == dst).head
      println(s"Edge ($src, $dst): $betweenness [${edge.weight}] => ${betweenness/edge.weight}")
    }
  }

  def process(graph: Graph, title: String, fileName: String): Unit = {
    //plotGraph(graph, title, fileName)
    process(graph)
  }
}
