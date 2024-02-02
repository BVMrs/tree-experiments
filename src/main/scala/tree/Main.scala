package tree

import scala.collection.mutable.ListBuffer
import scala.math.Ordering.Double.TotalOrdering

object Main {
  def main(args: Array[String]) = {
    implicit val ordering: Ordering[Double] = TotalOrdering

    val list0 = new ListBuffer[Long]
    val list1 = new ListBuffer[Long]

    list1.append(System.nanoTime())

    var avlTree: AVLTree[Double] = EmptyTree
    for (i <- 0 to 100_000_000) {
      if (i % 1000 == 0) {
        val currentTime = System.nanoTime()
        val lastTime = list1.last

        list1.append(System.nanoTime())
        println(s"inserted = ${i} \t\t inserts per s =\t\t ${ 1_000.0 / ((currentTime - lastTime) / 1_000_000_000.0)}\t\t diff ns =\t${currentTime - lastTime} ")
      }
      val value = Math.random() * 100_000_000
      val t0 = System.nanoTime()
      avlTree = AVLTree.insert(avlTree, value)
      val t1 = System.nanoTime()
      list0.append(t1 - t0)
    }
//
//    println(list0(99_000_000))

//    var avlTree: AVLTree[Int] = EmptyTree
//    avlTree = AVLTree.insert(avlTree, 10)
//    avlTree = AVLTree.insert(avlTree, 20)
//    avlTree = AVLTree.insert(avlTree, 30)
//    avlTree = AVLTree.insert(avlTree, 15)
//    avlTree = AVLTree.insert(avlTree, 14)
//    avlTree = AVLTree.insert(avlTree, 13)
//    avlTree = AVLTree.insert(avlTree, 12)
//    avlTree = AVLTree.insert(avlTree, 11)
//    AVLTree.printTree(avlTree)
  }
}