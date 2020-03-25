// Xiang Zhu
// DS4300 HW4 Part B: 5

/*
Implement a rudimentary graph database built upon your in-memory key-value store.
The graph database should be a single class with the following methods:

      addNode(v: String)
      addEdge(u: String, v: String)
      adjacent(v: String): List[String]
      shortestPath(u: String, v: String): List[String]
 */
class Graph {

  type Vertex = String
  type Graph = Map[Vertex,List[Vertex]]
  val keyVal = new Redis()
  var nodes = List[String]()

  // Add new node to graph.
  def addNode(v: String) = {
    if (nodes contains v)
      throw new Exception("Duplicate !!!")
    else
      nodes = nodes :+ v
  }

  //  adds an edge to graph between nodes u and v.
  def addEdge(u: String, v: String) = {
    if (!(nodes contains v)) addNode(v)
    if (!(nodes contains u)) addNode(u)
    keyVal.set(u, v)
    keyVal.set(v, u)
  }

  // Two vertices are said to be adjacent if there is an edge (arc) connecting them.
  // Returns a list of all nodes that connect to the given node
  def adjacent(v: String): List[String] = {
    keyVal.findValue(v)
  }

  // Find the shortest path form u to v.
  def shortestPath(u: String, v: String): List[String] = {
    var g: Map[String, List[String]] = Map()

    for (node <- nodes)
      g = g + (node -> keyVal.get(node))

    var visited = List[String]()
    var path = BFS(u, g)
    var llen = 0

    for (i <- 0 until path.length)
      if (path(i).contains(v))
        llen = i + 1

    for (j <- 0 until llen)
      if (path(j).contains(v))
        visited = visited :+ v
      else if (path(j).length == 1)
        visited = visited :+ path(j)(0)
      else
        for (k <- 0 until path(j).length) {
          if ((visited.length <= j) && (BFS(path(j)(k), g)(llen - j - 1).contains(v)))
            visited = visited :+ path(j)(k)
        }
    visited
  }

  // BFS algorithm
  def BFS(start: Vertex, g: Graph): List[List[Vertex]] = {
    def BFS0(elems: List[Vertex],visited: List[List[Vertex]]): List[List[Vertex]] = {
      val newNeighbors = elems.flatMap(g(_)).filterNot(visited.flatten.contains).distinct
      if (newNeighbors.isEmpty)
        visited
      else
        BFS0(newNeighbors, newNeighbors :: visited)
    }
    BFS0(List(start),List(List(start))).reverse
  }
}
