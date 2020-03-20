// Xiang Zhu
// DS4300 HW4 Part A: 3

import scala.math.min

object Partition extends App {

  // Return the fraction of records that would have to be re-assigned to a new node if
  // records were re-partitioned from startN to endN nodes using the mod function.
  // assigned_node = ID mod n (where n is the number of nodes in my network.)
  def moved(records: Int, startN: Int, endN: Int): Double = {
    var reassigned = 0
    for (i <-  min(startN, endN) to records) {
      val start_node = i % startN
      val end_node = i % endN
      if (start_node != end_node) {
        reassigned += 1
      }
    }
    reassigned / records.toDouble
  }
  // 0.990601
  println(moved(1000000, 100, 107))
}
