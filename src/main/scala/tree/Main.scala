package tree

import scala.collection.mutable.ListBuffer
import scala.math.Ordering.Double.TotalOrdering

object Main {
  def main(args: Array[String]) = {
    implicit val ordering: Ordering[Double] = TotalOrdering

    val list0 = new ListBuffer[Long]

    var avlTree: AVLTree[Double] = EmptyTree
    for (i <- 0 to 1_000_000) {
      val value = Math.random() * 100_000_000
      val t0 = System.nanoTime()
      avlTree = AVLTree.insert(avlTree, value)
      val t1 = System.nanoTime()
      list0.append(t1 - t0)
    }

    println(list0(99_000_000))
  }

  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] = {
      MyIO(() => f(unsafeRun()).unsafeRun())
    }
  }

  def getTime(): MyIO[Long] = MyIO(() => System.currentTimeMillis())

  def measure[B](computation: MyIO[B]): MyIO[Long] = for {
    time0 <- getTime()
    _ <- computation
    time1 <- getTime()
  } yield time1 - time0
}