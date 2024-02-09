package tree

import model.SelfBalancingTree

/**
 * AVL Tree is a self balancing binary search tree where the balance
 * factor of any node is bound to the interval [-1, 1]. Additionally,
 * any node added to the tree will have a balance factor of 0.
 *
 * A direct and interesting implication of this property is that the
 * tree's computeHeight grows logarithmically.
 *
 * The balance factor is defined as the value of:
 * BF = computeHeight(left_subtree) - computeHeight(right_subtree)
 *
 * When a new node is added to the tree, rebalancing is achieved by
 * rotating the tree.
 *
 * @tparam A
 */
sealed trait AVLTree[+A] extends SelfBalancingTree[A]
case object EmptyTree extends AVLTree[Nothing]
case class Node[A](value: A, left: AVLTree[A], right: AVLTree[A], height: Int) extends AVLTree[A]

object AVLTree {
  def apply[A](value: A): AVLTree[A] = new Node(value, EmptyTree, EmptyTree, 1)

  /**
   * Gets the reference to an AVL Tree node instance if it exists in the tree.
   * Otherwise returns empty tree.
   *
   * @param root
   * @param qValue
   * @param ord
   * @tparam A
   * @return node reference.
   */
  def getNode[A](root: AVLTree[A], qValue: A)(implicit ord: Ordering[A]): AVLTree[A] = root match {
    case node @ Node(nValue, left, right, _) => {
      val cmpRes = ord.compare(nValue, qValue)

      if (cmpRes == 0) {
        node
      } else if (cmpRes == 1 ) {
        getNode(left, qValue)(ord)
      } else {
        getNode(right, qValue)(ord)
      }
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
   * References
   * 1. https://www.programiz.com/dsa/avl-tree
   *
   * @param node
   * @param tree
   * @tparam A
   * @return
   */
  def insert[A](tree: AVLTree[A], newValue: A)(implicit ord: Ordering[A]): AVLTree[A] = tree match {
    case pNode@Node(pValue, left, right, _) => {
      val res = ord.compare(pValue, newValue)

      if (res == -1) {
        val newRight = insert(right, newValue)(ord)
        val newTree = Node(pValue, left, newRight, 1 + Math.max(computeHeight(left), computeHeight(newRight)))
        balance(newTree)(ord)
      } else if (res == 1) {
        val newLeft = insert(left, newValue)(ord)
        val newTree = Node(pValue, newLeft, right, 1 + Math.max(computeHeight(newLeft), computeHeight(right)))
        balance(newTree)(ord)
      } else {
        pNode
      }
    }
    case _ => Node(newValue, EmptyTree, EmptyTree, 1)
  }

  def delete[A](tree: AVLTree[A]) = ???

  /**
   * Print the AVL Tree in-order
   *
   * @param tree
   * @tparam A
   */
  def debugPrint[A](tree: AVLTree[A]): Unit = tree match {
    case EmptyTree =>
    case Node(value, left, right, height) =>
      debugPrint(left)
      println(s"${value} ${height}")
      debugPrint(right)
  }

  /**
   * Balance factor is checked for the newly added node.
   * The tree's definition enforces an balance factor of 0 for any newly added
   * node. Rotations are required to restore the tree.
   *
   * @param tree
   * @tparam A
   * @return
   */
  private def balance[A](tree: AVLTree[A])(implicit ord: Ordering[A]): AVLTree[A] = tree match {
    case EmptyTree => EmptyTree
    case node @ Node(value, left, right, _) => {
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

  /**
   * Compute the balance factor for a given node. Although the definition of an
   * AVL Tree is bound to this measurement, we'll only store the heights of the
   * tree and compute this factor whenever we need it.
   *
   * @param tree
   * @tparam A
   * @return
   */
  private def balanceFactor[A](tree: AVLTree[A]): Int = tree match {
    case EmptyTree => 0
    case Node(_,
      Node(_, _, _, leftHeight),
      EmptyTree, _) => leftHeight
    case Node(_,
      EmptyTree,
      Node(_, _, _, rightHeight), _) => -rightHeight
    case Node(_,
      Node(_, _, _, leftHeight),
      Node(_, _, _, rightHeight), _) => leftHeight - rightHeight
  }

  /**
   * Utility function to compute the height of a given node in the tree.
   *
   * @param tree
   * @tparam A
   * @return
   */
  private def computeHeight[A](tree: AVLTree[A]): Int = tree match {
    case EmptyTree => 0
    case Node(_, left, right, _) => 1 + math.max(computeHeight(left), computeHeight(right))
  }

  /**
   * Performs a transformation on the node that is supplied as an input.
   * If the function receives x as an input and y is the node we are rotating
   * around, then the result will be the following:
   *
   *       a      |        a
   *      /       |       /
   *     x        |      y
   *    / \       |     /  \
   *   b   y      |    x    d
   *      / \     |   /  \
   *      c  d    |  b   c
   *              |
   * @param tree
   * @tparam A
   * @return
   */
  private def rotateLeft[A](tree: AVLTree[A]): AVLTree[A] = tree match {
    case Node(pivotValue, left, Node(rightValue, rightLeft, rightRight, _), _) =>
      val newHeight = 1 + Math.max(computeHeight(left), computeHeight(rightLeft))
      val newLeft = Node(pivotValue, left, rightLeft, newHeight)
      Node(rightValue, newLeft, rightRight, 1 + Math.max(newHeight, computeHeight(rightRight)))
    case _ => EmptyTree
  }

  /**
   * Performs a transformation on the node that is supplied as an input.
   * If the function receives y as an input and x is the node we are rotating
   * around, then the result will be the following:
   *
   *         a    |       a
   *        /     |      /
   *       y      |     x
   *      /  \    |    / \
   *     x    d   |   b   y
   *    /  \      |      / \
   *   b   c      |      c  d
   *              |
   * @param tree
   * @tparam A
   * @return
   */
  private def rotateRight[A](tree: AVLTree[A]): AVLTree[A] = tree match {
    case Node(pivotValue, Node(leftValue, leftLeft, leftRight, _), right, _) =>
      val newHeight = 1 + Math.max(computeHeight(right), computeHeight(leftRight))
      val newRight = Node(pivotValue, leftRight, right, newHeight)
      Node(leftValue, leftLeft, newRight, 1 + Math.max(newHeight, computeHeight(leftLeft)))
    case _ => EmptyTree
  }

  /**
   * Performs a transformation on the node that is supplied as an input.
   * If the function receives y as an input and z is the node we are rotating
   * around, then the result will be the following:
   *
   * Left Right Rotation | Rotate left x - y | Rotate right y - z
   *          a          |         a         |         a
   *          |          |         |         |         |
   *          z          |         z         |         y
   *         / \         |        / \        |        / \
   *        x   b        |       y   b       |       /   \
   *       / \           |      / \          |      x     z
   *      c   y          |     x   e         |     / \   / \
   *         / \         |    / \            |    c   d e   b
   *        d   e        |   c   d           |
   *                     |                   |
   *
   * @param tree
   * @tparam A
   * @return
   */
  private def rotateLeftRight[A](tree: AVLTree[A]): AVLTree[A] = tree match {
    case Node(rootValue, left, right, height) => {
      val tmpRoot = rotateLeft(left)
      rotateRight(
        // height does not matter. it will be updated shortly.
        Node(rootValue, tmpRoot, right, height)
      )
    }
    case _ => EmptyTree
  }

  /**
   * Right Left Rotation | Rotate right x - y | Rotate left y - z
   *          a          |         a          |         a
   *          |          |         |          |         |
   *          z          |         z          |         y
   *         / \         |        / \         |        / \
   *        b   x        |       b   y        |       /   \
   *           / \       |          / \       |      z     x
   *          y   c      |         e   x      |     / \   / \
   *         / \         |            / \     |    b   e d   c
   *        d   e        |           d   c    |
   *
   * @param tree
   * @tparam A
   * @return
   */
  private def rotateRightLeft[A](tree: AVLTree[A]): AVLTree[A] = tree match {
    case Node(rootValue, left, right, height) => {
      val tmpRoot = rotateRight(right)
      rotateLeft(
        Node(rootValue, left, tmpRoot, height)
      )
    }
    case _ => EmptyTree
  }
}