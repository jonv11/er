package org.example

import scala.collection.mutable

object LeidenAlgorithm {

  import GraphSchema._

  /**
   * Represents a community.
   *
   * @param id        Community ID.
   * @param vertices  Set of vertex IDs in the community.
   * @param edges     Sequence of edges in the community.
   */
  case class Community(id: Int, vertices: Set[Int], edges: Seq[Edge])

    /**
     * Calculate the betweenness centrality for each vertex in the graph.
     *
     * @param graph The input graph.
     * @return A map of vertex IDs to their betweenness centrality values.
     */
    def calculateBetweennessCentrality(graph: Graph): Map[Int, Double] = {
      val betweenness = mutable.Map[Int, Double]().withDefaultValue(0.0)

      for (source <- graph.vertices) {
        val stack = mutable.Stack[Int]()
        val predecessors = mutable.Map[Int, List[Int]]().withDefaultValue(Nil)
        val shortestPaths = mutable.Map[Int, Double]().withDefaultValue(0.0)
        val distances = mutable.Map[Int, Int]().withDefaultValue(-1)
        val dependencies = mutable.Map[Int, Double]().withDefaultValue(0.0)

        val queue = mutable.Queue[Int]()
        queue.enqueue(source)
        distances(source) = 0
        shortestPaths(source) = 1

        while (queue.nonEmpty) {
          val v = queue.dequeue()
          stack.push(v)

          for (edge <- graph.edges if edge.src == v || edge.dst == v) {
            val w = if (edge.src == v) edge.dst else edge.src

            if (distances(w) == -1) {
              queue.enqueue(w)
              distances(w) = distances(v) + 1
            }

            if (distances(w) == distances(v) + 1) {
              shortestPaths(w) += shortestPaths(v)
              predecessors(w) ::= v
            }
          }
        }

        while (stack.nonEmpty) {
          val w = stack.pop()

          for (v <- predecessors(w)) {
            dependencies(v) += (shortestPaths(v) / shortestPaths(w)) * (1.0 + dependencies(w))
          }

          if (w != source) {
            betweenness(w) += dependencies(w)
          }
        }
      }

      betweenness.toMap
    }

    /**
     * Calculate the modularity gain for moving a vertex to a community, considering betweenness centrality.
     *
     * @param vertex    The vertex to move.
     * @param community The target community.
     * @param graph     The input graph.
     * @param betweenness A map of vertex IDs to their betweenness centrality values.
     * @return The modularity gain.
     */
    def calculateModularityGain(vertex: Int, community: Community, graph: Graph, betweenness: Map[Int, Double]): Double = {
      val vertexDegree = graph.edges.count(e => e.src == vertex || e.dst == vertex)
      val communityDegree = community.vertices.map(v => graph.edges.count(e => e.src == v || e.dst == v)).sum
      val totalEdges = graph.edges.size
      val vertexBetweenness = betweenness.getOrElse(vertex, 0.0)

      // Modularity gain calculation
      val modularityGain = (vertexDegree.toDouble / (2 * totalEdges)) -
        (communityDegree.toDouble * vertexDegree.toDouble / (4 * totalEdges * totalEdges)) +
        vertexBetweenness

      //println(s"Modularity gain for vertex $vertex moving to community ${community.id}: $modularityGain")
      modularityGain
    }

    /**
     * Move nodes to neighboring communities to optimize modularity.
     *
     * @param graph       The input graph.
     * @param communities Current communities.
     * @return Updated communities.
     */
    def localMoving(graph: Graph, communities: Map[Int, Community]): Map[Int, Community] = {
      val betweenness = calculateBetweennessCentrality(graph)
      var updatedCommunities = communities
      var improvement = true

      while (improvement) {
        improvement = false

        for (vertex <- graph.vertices) {
          val currentCommunity = updatedCommunities(vertex)
          val neighboringCommunities = graph.edges.filter(e => e.src == vertex || e.dst == vertex)
            .flatMap(e => Seq(updatedCommunities(e.src), updatedCommunities(e.dst)))
            .distinct

          //println(s"Vertex $vertex current community: ${currentCommunity.id}, neighboring communities: ${neighboringCommunities.map(_.id).mkString(", ")}")

          if (neighboringCommunities.nonEmpty) {
            val bestCommunity = neighboringCommunities.maxBy { community =>
              val modularityGain = calculateModularityGain(vertex, community, graph, betweenness)
              modularityGain
            }

            if (bestCommunity.id != currentCommunity.id) {
              // Move vertex to the best community
              updatedCommunities = updatedCommunities.updated(bestCommunity.id, Community(
                id = bestCommunity.id,
                vertices = bestCommunity.vertices + vertex,
                edges = bestCommunity.edges ++ graph.edges.filter(e => e.src == vertex || e.dst == vertex)
              ))

              // Remove vertex from the current community
              updatedCommunities = updatedCommunities.updated(currentCommunity.id, Community(
                id = currentCommunity.id,
                vertices = currentCommunity.vertices - vertex,
                edges = currentCommunity.edges.filter(e => e.src != vertex && e.dst != vertex)
              ))

              //println(s"Vertex $vertex moved to community ${bestCommunity.id}")
              improvement = true
            }
          }
        }
      }
      updatedCommunities
    }

    /**
     * Calculate the modularity gain for merging two communities.
     *
     * @param community1 The first community.
     * @param community2 The second community.
     * @param graph      The input graph.
     * @param betweenness A map of vertex IDs to their betweenness centrality values.
     * @return The modularity gain.
     */
    def calculateMergeModularityGain(community1: Community, community2: Community, graph: Graph, betweenness: Map[Int, Double]): Double = {
      val community1Degree = community1.vertices.map(v => graph.edges.count(e => e.src == v || e.dst == v)).sum
      val community2Degree = community2.vertices.map(v => graph.edges.count(e => e.src == v || e.dst == v)).sum
      val totalEdges = graph.edges.size
      val betweennessSum = community1.vertices.map(betweenness.getOrElse(_, 0.0)).sum + community2.vertices.map(betweenness.getOrElse(_, 0.0)).sum

      // Modularity gain calculation for merging communities considering betweenness centrality
      val modularityGain = ((community1Degree + community2Degree).toDouble / (2 * totalEdges)) -
        ((community1Degree.toDouble / (2 * totalEdges)) * (community2Degree.toDouble / (2 * totalEdges))) + betweennessSum

      //println(s"Modularity gain for merging communities ${community1.id} and ${community2.id}: $modularityGain")
      modularityGain
    }

    /**
     * Refine communities by merging them based on local movements.
     *
     * @param graph       The input graph.
     * @param communities Current communities.
     * @return Refined communities.
     */
    def refineCommunities(graph: Graph, communities: Map[Int, Community]): Map[Int, Community] = {
      val betweenness = calculateBetweennessCentrality(graph)
      var refinedCommunities = communities
      var improvement = true

      while (improvement) {
        improvement = false

        for ((id, community) <- refinedCommunities) {
          val neighboringCommunities = community.edges
            .flatMap(e => Seq(refinedCommunities(e.src), refinedCommunities(e.dst)))
            .distinct

          //println(s"Community $id neighboring communities: ${neighboringCommunities.map(_.id).mkString(", ")}")

          if (neighboringCommunities.nonEmpty) {
            val bestCommunity = neighboringCommunities.maxBy { neighbor =>
              val modularityGain = calculateMergeModularityGain(community, neighbor, graph, betweenness)
              modularityGain
            }

            if (bestCommunity.id != community.id) {
              // Merge communities
              refinedCommunities = refinedCommunities - id
              refinedCommunities = refinedCommunities.updated(bestCommunity.id, Community(
                id = bestCommunity.id,
                vertices = bestCommunity.vertices ++ community.vertices,
                edges = bestCommunity.edges ++ community.edges
              ))

              //println(s"Communities ${community.id} and ${bestCommunity.id} merged")
              improvement = true
            }
          }
        }
      }
      refinedCommunities
    }

    /**
     * Initialize communities with each vertex in its own community.
     *
     * @param graph The input graph.
     * @return Initial communities.
     */
    def initializeCommunities(graph: Graph): Map[Int, Community] = {
      graph.vertices.map(v => v -> Community(v, Set(v), Seq.empty)).toMap
    }

    /**
     * Run the Leiden algorithm on the input graph.
     *
     * @param graph The input graph.
     * @return Detected communities.
     */
    def run(graph: Graph): Map[Int, Community] = {
      val initialCommunities = initializeCommunities(graph)
//      println("Initial Communities:")
//      initialCommunities.foreach { case (id, community) =>
//        println(s"Community ID: $id, Vertices: ${community.vertices.mkString(", ")}")
//      }

      val locallyMovedCommunities = localMoving(graph, initialCommunities)
//      println("Communities after local moving phase:")
//      locallyMovedCommunities.foreach { case (id, community) =>
//        println(s"Community ID: $id, Vertices: ${community.vertices.mkString(", ")}")
//      }

      val refinedCommunities = refineCommunities(graph, locallyMovedCommunities)
//      println("Communities after refinement phase:")
//      refinedCommunities.foreach { case (id, community) =>
//        println(s"Community ID: $id, Vertices: ${community.vertices.mkString(", ")}")
//      }

      refinedCommunities
    }
  }


