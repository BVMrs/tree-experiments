package tree

import model.tree.SelfBalancingTree

sealed trait BPlusTree[+A] extends SelfBalancingTree[A]
case object EmptyTree extends BPlusTree[Nothing]
case class Node[A](value: A, left: BPlusTree[A], right: BPlusTree[A], height: Int) extends BPlusTree[A]

object BTree {

}
