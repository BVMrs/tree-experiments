package tree

import model.SelfBalancingTree

/**
 * AVL Tree is a self balancing binary search tree where the balance
 * factor of any node is bound to the interval [-1, 1]. Additionally,
 * any node added to the tree will have a balance factor of 0.
 *
 * A direct and interesting implication of this property is that the
 * tree's height grows logarithmically.
 *
 * The balance factor is defined as the value of:
 * BF = height(left_subtree) - height(right_subtree)
 *
 * When a new node is added to the tree, rebalancing is achieved by
 * rotating the tree.
 *
 * @tparam A
 */
sealed trait AVLTree[+A] extends SelfBalancingTree[A]
case object EmptyTree extends AVLTree[Nothing]
case class Node[A](value: A, left: AVLTree[A], right: AVLTree[A]) extends AVLTree[A]

object AVLTree {
  private def balanceFactor[A](tree: AVLTree[A]): Int = tree match {
    case EmptyTree => 0
    case Node(_, left, right) => height(left) - height(right)
  }

  private def height[A](tree: AVLTree[A]): Int = tree match {
    case EmptyTree => 0
    case Node(_, left, right) => 1 + math.max(height(left), height(right))
  }

  //     Rotate x - y left      |
  //       a      |        a    |
  //      /       |       /     |
  //     x        |      y      |
  //    / \       |     /  \    |
  //   b   y      |    x    d   |
  //      / \     |   /  \      |
  //      c  d    |  b   c      |
  //              |             |
  private def rotateLeft[A](tree: AVLTree[A]): AVLTree[A] = tree match {
    case Node(pivotValue, left, Node(rightValue, rightLeft, rightRight)) =>
      Node(rightValue, Node(pivotValue, left, rightLeft), rightRight)
    case _ => EmptyTree
  }

  //  private def rotateRight[A](y: AVLTree[A]): AVLTree[A] = y match {
  //    case Node(root, Node(leftValue, leftLeft, leftRight, _), right, _) => {
  //      val newHeight = 1 + Math.max(height(leftRight), height(right))
  //      Node(
  //        leftValue,
  //        leftLeft,
  //        Node(root, leftRight, right, newHeight),
  //        1 + Math.max(newHeight, height(leftLeft))
  //      )
  //    }
  //    case _ => EmptyTree
  //  }

  //     Rotate x - y right      |
  //         a    |       a      |
  //        /     |      /       |
  //       y      |     x        |
  //      /  \    |    / \       |
  //     x    d   |   b   y      |
  //    /  \      |      / \     |
  //   b   c      |      c  d    |
  //              |              |
  private def rotateRight[A](tree: AVLTree[A]): AVLTree[A] = tree match {
    case Node(pivotValue, Node(leftValue, leftLeft, leftRight), right) =>
      Node(leftValue, leftLeft, Node(pivotValue, leftRight, right))
    case _ => EmptyTree
  }


  // Left Right Rotation | Rotate left x - y | Rotate right y - z |
  //          a          |         a         |         a          |
  //          |          |         |         |         |          |
  //          z          |         z         |         y          |
  //         / \         |        / \        |        / \         |
  //        x   b        |       y   b       |       /   \        |
  //       / \           |      / \          |      x     z       |
  //      c   y          |     x   e         |     / \   / \      |
  //         / \         |    / \            |    c   d e   b     |
  //        d   e        |   c   d           |                    |
  //                     |                   |                    |
  private def rotateLeftRight[A](tree: AVLTree[A]): AVLTree[A] = tree match {
    case Node(rootValue, left, right) => {
      val tmpRoot = rotateLeft(left)
      rotateRight(
        Node(rootValue, tmpRoot, right)
      )
    }
    case _ => EmptyTree
  }

  // Right Left Rotation | Rotate right x - y | Rotate left y - z |
  //          a          |         a          |         a         |
  //          |          |         |          |         |         |
  //          z          |         z          |         y         |
  //         / \         |        / \         |        / \        |
  //        b   x        |       b   y        |       /   \       |
  //           / \       |          / \       |      z     x      |
  //          y   c      |         e   x      |     / \   / \     |
  //         / \         |            / \     |    b   e d   c    |
  //        d   e        |           d   c    |                   |                          |
  //                     |                    |                   |
  private def rotateRightLeft[A](tree: AVLTree[A]): AVLTree[A] = tree match {
    case Node(rootValue, left, right) => {
      val tmpRoot = rotateRight(right)
      rotateLeft(
        Node(rootValue, left, tmpRoot)
      )
    }
    case _ => EmptyTree
  }

  /**
   * Algorithm for inserting a node:
   *   1. Perform a search through the BST to find the location where the
   *      new node will be added by calling this function recursively. The
   *      tree is ordered according to a given ordering criteria. A new node in
   *      an AVL tree will always be a leaf node before rebalancing.
   *      2. When a leaf node
   *
   * https://www.programiz.com/dsa/avl-tree
   *
   * @param node
   * @param tree
   * @tparam A
   * @return
   */
  def insert[A](tree: AVLTree[A], newValue: A)(implicit ord: Ordering[A]): AVLTree[A] = tree match {
    case pNode@Node(pValue, left, right) => {
      val res = ord.compare(pValue, newValue)

      if (res == -1) {
        val newRight = insert(right, newValue)(ord)
        val newTree = Node(pValue, left, newRight)
        balance(newTree)(ord)
      } else if (res == 1) {
        val newLeft = insert(left, newValue)(ord)
        val newTree = Node(pValue, newLeft, right)
        balance(newTree)(ord)
      } else {
        pNode
      }
    }
    case _ => Node(newValue, EmptyTree, EmptyTree)
  }

  // Print the AVL Tree in-order
  def printTree[A](tree: AVLTree[A]): Unit = tree match {
    case EmptyTree =>
    case Node(value, left, right) =>
      println(value)
      printTree(left)
      printTree(right)
  }

  /**
   * Balance factor is checked for the newly added node
   *
   * @param tree
   * @tparam A
   * @return
   */
  private def balance[A](tree: AVLTree[A])(implicit ord: Ordering[A]): AVLTree[A] = tree match {
    case EmptyTree => EmptyTree
    case node@Node(value, left, right) => {
      val bf = balanceFactor(tree)

      if (bf > 1) /* left tree is dominant */ {
        if (balanceFactor(left) > 0)
          rotateRight(tree)
        else
          rotateLeftRight(tree)
      } else if (bf < -1) /* right tree is dominant */ {
        if (balanceFactor(right) < 0)
          rotateLeft(tree)
        else
          rotateRightLeft(tree)
      } else {
        tree
      }
    }
  }
}