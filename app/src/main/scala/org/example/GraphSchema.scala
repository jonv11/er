package org.example

object GraphSchema {

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

}
