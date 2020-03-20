// Xiang Zhu
// March 20, 2020

object HW4 extends App{

  override def main(args: Array[String]) {

    val g: Graph = new Graph()

    g.addNode("X")
    g.addNode("E")
    g.addNode("F")
    g.addNode("J")
    g.addNode("R")
    g.addNode("C")
    g.addNode("B")
    g.addNode("Y")

    g.addEdge("X", "J")
    g.addEdge("F", "E")
    g.addEdge("F", "B")
    g.addEdge("F", "J")
    g.addEdge("R", "Y")
    g.addEdge("R", "E")
    g.addEdge("R", "C")
    g.addEdge("R", "B")
    g.addEdge("R", "J")
    g.addEdge("J", "F")
    g.addEdge("J", "R")
    g.addEdge("J", "B")
    g.addEdge("J", "X")
    g.addEdge("C", "R")
    g.addEdge("C", "B")
    g.addEdge("B", "R")
    g.addEdge("B", "C")
    g.addEdge("B", "J")
    g.addEdge("E", "F")
    g.addEdge("E", "Y")
    g.addEdge("E", "R")
    g.addEdge("Y", "R")
    g.addEdge("Y", "E")


    val path = g.shortestPath("X", "Y")
    path.foreach(println)


  }

  // Practice code
  /*
  type Vertex = String
  type Graph = Map[Vertex,List[Vertex]]
  val g: Graph= Map("X" -> List("J"),
    "J" -> List("B", "X","R","F"),
    "B" -> List("C","J","R","F"),
    "R" -> List("B", "C", "J", "Y", "E"),
    "F" -> List("B", "J", "E"),
    "C" -> List("B", "R"),
    "E" -> List("R", "F", "Y"),
    "Y" -> List("E", "R"))
  //example graph meant to represent
  //  1---2
  //  |   |
  //  4---3

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

  println(BFS("X", g))
  println(BFS("Y", g).reverse)

  var acc = List[String]()
  var start = BFS("X", g)
  var end = BFS("Y", g).reverse

  for (i <- 0 until end.length) {
    if (end(i).contains("X"))
      for (j <- 0 until start.length)
        if (start(j).contains("Y"))
          acc = acc :+ "Y"
        else if (start(j).length == 1)
          acc = acc :+ start(j)(0)
        else
          for (k <- 0 until start(j).length) {
            if (end(i + j).contains(start(j)(k)))
              acc = acc :+ start(j)(k)
          }
  }

  println(acc)

  */
}