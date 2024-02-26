package jmh

import org.openjdk.jmh.annotations.Benchmark
import tree.{AVLTree, EmptyTree}

class bench_AVL {
  @Benchmark
  def benchmarkAVL(): Unit = {
    implicit val ordering: Ordering[Int] = Ordering.Int

    //    jmh:run -rf json -rff avl_tree_results_1.json -prof jfr

    //    var avlTree: AVLTree[Int] = EmptyTree
    //    avlTree = AVLTree.insert(avlTree, 10)
    //    avlTree = AVLTree.insert(avlTree, 20)
    //    avlTree = AVLTree.insert(avlTree, 30)
    //    avlTree = AVLTree.insert(avlTree, 15)
    //    avlTree = AVLTree.insert(avlTree, 14)
    //    avlTree = AVLTree.insert(avlTree, 13)
    //    avlTree = AVLTree.insert(avlTree, 12)
    //    avlTree = AVLTree.insert(avlTree, 11)
    var avlTree: AVLTree[Double] = EmptyTree
    for (i <- 0 to 100_000) {
      val value = Math.random() * 1_000_000
      avlTree = AVLTree.insert(avlTree, value)
    }
  }
}