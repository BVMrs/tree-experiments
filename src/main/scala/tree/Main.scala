package tree

import scala.collection.mutable.ListBuffer
import scala.math.Ordering.Double.TotalOrdering

object Main {
  def main(args: Array[String]): Unit = {
    implicit val ordering: Ordering[Double] = TotalOrdering

    val list0 = new ListBuffer[Long]
    val list1 = new ListBuffer[Long]

    list1.append(System.nanoTime())

    //TODO: Benchmarking method is rather lousy. Looking for a better alternative
    var avlTree: AVLTree[Double] = EmptyTree
    for (i <- 0 to 100_000_000) {
      if (i % 1000 == 0) {
        val currentTime = System.nanoTime()
        val lastTime = list1.last

        list1.append(System.nanoTime())
        println(s"ti=${i}|ips=${ 1_000.0 / ((currentTime - lastTime) / 1_000_000_000.0)}|diff_ns=${currentTime - lastTime}")
      }
      val value = Math.random() * 100_000_000
      val t0 = System.nanoTime()
      avlTree = AVLTree.insert(avlTree, value)
      val t1 = System.nanoTime()
      list0.append(t1 - t0)
    }
  }
}