package org.example

import org.example.GraphSchema._

import scala.collection.mutable

/**
 * EdgeBetweennessCentrality calculates the edge betweenness centrality for a graph.
 */
object EdgeBetweennessCentrality {

  /**
   * Calculates the shortest paths from a given source vertex to all other vertices in the graph.
   *
   * @param graph  The graph.
   * @param source The source vertex ID.
   * @return A map where the key is the vertex ID and the value is a sequence of predecessor vertices.
   */
  def calculateShortestPaths(graph: Graph, source: Int): Map[Int, Seq[Int]] = {
    // Initialize a mutable map to store the distances from the source to each vertex.
    // Initially, set all distances to positive infinity.
    val distances = mutable.Map[Int, Double]()

    // Initialize a mutable map to store the predecessor vertices for each vertex.
    val predecessors = mutable.Map[Int, Seq[Int]]()

    // Set initial distances and predecessors for all vertices.
    graph.vertices.foreach { v =>
      distances(v) = Double.PositiveInfinity
      predecessors(v) = Seq()
    }

    // Set the distance from the source to itself to 0.
    distances(source) = 0.0

    // Create a priority queue to process vertices based on their distance.
    val queue = mutable.PriorityQueue[(Double, Int)]()(Ordering.by(-_._1))
    queue.enqueue((0.0, source))

    // Process the priority queue until it's empty.
    while (queue.nonEmpty) {
      // Dequeue the vertex with the smallest distance.
      val (currentDist, currentVertex) = queue.dequeue()

      // If the current distance is less than or equal to the stored distance, proceed.
      if (currentDist <= distances(currentVertex)) {
        // Iterate over each edge connected to the current vertex.
        for (Edge(_, neighbor, weight) <- graph.edges if graph.edges.contains(Edge(currentVertex, neighbor, weight))) {
          // Calculate the distance to the neighbor vertex.
          val distance = currentDist + 1

          // If the new distance is shorter, update the distance and predecessors.
          if (distance < distances(neighbor)) {
            distances(neighbor) = distance
            predecessors(neighbor) = Seq(currentVertex)
            queue.enqueue((distance, neighbor))
          } else if (distance == distances(neighbor)) {
            // If the distance is equal, add the current vertex to the predecessors.
            predecessors(neighbor) :+= currentVertex
          }
        }
      }
    }

    // Convert the mutable map to an immutable map and return it.
    predecessors.toMap
  }


  /**
   * Accumulates the edge betweenness centrality for all edges in the graph.
   *
   * @param graph The graph.
   * @return A map where the key is a tuple of source and destination vertex IDs, and the value is the edge betweenness centrality.
   */
  def accumulateEdgeBetweenness(graph: Graph): Map[(Int, Int), Double] = {
    // Initialize a mutable map to store the edge betweenness centrality values
    val edgeBetweenness = mutable.Map[(Int, Int), Double]().withDefaultValue(0.0)

    // Iterate over each vertex in the graph as the source vertex
    for (source <- graph.vertices) {
      // Calculate the shortest paths from the source vertex to all other vertices
      val shortestPaths = calculateShortestPaths(graph, source)

      // Initialize a mutable map to store the dependency values for each vertex
      val dependencies = mutable.Map[Int, Double]().withDefaultValue(0.0)

      // Sort vertices based on the number of shortest paths they are involved in
      var nodes = graph.vertices.toSeq.sortBy(n => shortestPaths(n).size)

      // Process each node in reverse order (starting from the farthest)
      while (nodes.nonEmpty) {
        val w = nodes.last       // Get the last node in the sorted list
        nodes = nodes.init       // Remove the last node from the list

        // Iterate over each predecessor of the current node in the shortest paths
        for (v <- shortestPaths(w)) {
          // Calculate the dependency ratio for the current node
          val ratio = (1.0 + dependencies(w)) / shortestPaths(w).size

          // Accumulate the dependency value for the predecessor
          dependencies(v) += ratio

          // Ensure the edge is represented in a consistent order (v, w)
          val edge = if (v < w) (v, w) else (w, v)

          // Accumulate the edge betweenness centrality value for the edge
          edgeBetweenness(edge) += ratio
        }
      }
    }

    // Convert the mutable map to an immutable map and return it
    edgeBetweenness.toMap
  }


  /**
   * Main method to run the Edge Betweenness Centrality calculation.
   *
   * @param args Command-line arguments (not used).
   */
  def main(args: Array[String]): Unit = {

    // Girvan-Newman
    // https://en.wikipedia.org/wiki/Girvan%E2%80%93Newman_algorithm
    // Louvain
    // https://en.wikipedia.org/wiki/Louvain_method
    // https://neo4j.com/docs/graph-data-science/current/algorithms/louvain/
    // Leiden
    // https://en.wikipedia.org/wiki/Leiden_algorithm


    // https://www.youtube.com/watch?v=HYSfO7dI0c8
    // https://www.youtube.com/watch?v=nPwfqOf9KhM
    // https://www.youtube.com/watch?v=LtQoPEKKRYM
    // https://www.youtube.com/watch?v=Xt0vBtBY2BU
    // https://www.youtube.com/watch?v=3-hyXIcSHkA
    // https://www.youtube.com/watch?v=EYe2SQMvUYw

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

  }

  def process(graph: Graph): Unit = {
    //val communities = LeidenAlgorithm.run(graph)
    val edgeBetweenness = accumulateEdgeBetweenness(graph)
/*
    println("Communities:")
    for ((communityId, community) <- communities) {
      println(s"Community ID: $communityId")
      println(s"Vertices: ${community.vertices.mkString(", ")}")
      println(s"Edges: ${community.edges.map(e => s"(${e.src}, ${e.dst})").mkString(", ")}")
    }
*/
    println("Edge Betweenness Centrality:")
    val sorted = edgeBetweenness.map{
      case ((src, dst), betweenness) => (graph.edges.filter(e => e.src == src && e.dst == dst).head, betweenness)
    }.toSeq.sortBy { case (edge, betweenness) => -betweenness / edge.weight }

    def mean(seq: Seq[Double]): Double = seq.sum / seq.size

    def median(seq: Seq[Double]): Double = {
      val sortedSeq = seq.sorted
      val size = sortedSeq.size
      if (size % 2 == 0) {
        (sortedSeq(size / 2 - 1) + sortedSeq(size / 2)) / 2.0
      } else {
        sortedSeq(size / 2)
      }
    }

    def standardDeviation(seq: Seq[Double]): Double = {
      val avg = mean(seq)
      val variance = seq.map(x => math.pow(x - avg, 2)).sum / seq.size
      math.sqrt(variance)
    }

    val data = sorted.map{ case (edge, betweenness) => betweenness / edge.weight }

    val avg = mean(data)
    val med = median(data)
    val stdDev = standardDeviation(data)

    println(f"Mean: $avg%.2f")
    println(f"Median: $med%.2f")
    println(f"Standard Deviation: $stdDev%.2f")

    sorted.foreach { case (edge, betweenness) =>
      println(f"Edge (${edge.src}, ${edge.dst}): $betweenness%.2f [${edge.weight}%.2f] => ${betweenness / edge.weight}%.2f")
    }

    println("-----------------------")
  }

  def process(graph: Graph, title: String, fileName: String): Unit = {
    plotGraph(graph, title, fileName)
    process(graph)
  }


  private def plotGraph(graph: Graph, title: String, fileName: String): Unit = {

    import org.graphstream.graph._
    import org.graphstream.graph.implementations._
    import org.graphstream.stream.file.FileSinkImages
    import org.graphstream.stream.file.FileSinkImages.{LayoutPolicy, OutputType, Quality, Resolutions}

    System.setProperty("org.graphstream.ui", "org.graphstream.ui.j2dviewer.J2DGraphRenderer")
    val gsGraph = new SingleGraph(title)

    // Add nodes to the graph
    graph.vertices.foreach { v =>
      val node = gsGraph.addNode[Node](v.toString)
      node.setAttribute("ui.label", v.toString)
    }

    // Add edges to the graph
    accumulateEdgeBetweenness(graph).map{
      case ((src, dst), betweenness) => (graph.edges.filter(e => e.src == src && e.dst == dst).head, betweenness)
    }.foreach { case (e, betweenness) =>
      val score = betweenness / e.weight
      val edge = gsGraph.addEdge[org.graphstream.graph.Edge](s"${e.src}-${e.dst}", e.src.toString, e.dst.toString, false)
      edge.setAttribute("ui.label", f"$betweenness%.2f / ${e.weight}%.2f = $score%.2f")
    }

    // Add styles for nodes and edges
    gsGraph.addAttribute("ui.stylesheet", """
    node { fill-color: black; text-alignment: above; text-size: 20; }
    edge { text-alignment: above; text-size: 20; }
  """)
    gsGraph.addAttribute("ui.quality")
    gsGraph.addAttribute("ui.antialias")

    val pic = new FileSinkImages(OutputType.PNG, Resolutions.HD1080)
    pic.setLayoutPolicy(LayoutPolicy.COMPUTED_FULLY_AT_NEW_IMAGE)
    pic.setQuality(Quality.HIGH)
    pic.writeAll(gsGraph, fileName)

  }
}
