package org.example

import scala.collection.mutable
import org.graphstream.graph._
import org.graphstream.graph.implementations._
import org.graphstream.stream.file.FileSinkImages
import org.graphstream.stream.file.FileSinkImages.{LayoutPolicy, OutputType, Quality, Resolutions}
import org.graphstream.ui.swingViewer.ViewPanel
import org.graphstream.ui.view.Viewer

import java.awt.Graphics2D
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io.File

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
          val distance = currentDist + 1

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

  }

  def process(graph: Graph): Unit = {
    val edgeBetweenness = accumulateEdgeBetweenness(graph)

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
