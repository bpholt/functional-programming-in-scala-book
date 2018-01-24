package com.planetholt.fpinscala.datastructures

import org.scalatest.{Matchers, WordSpec}

class TreeSpec extends WordSpec with Matchers {

  "A tree" when {
    "without fold" should {
      import FoldlessTreeOps._

      "should use Tree.size to calculate the size of the tree" in {
        Branch(Leaf(1), Leaf(2)).size should be(3)
        Branch(Leaf(1), Branch(Leaf(2), Leaf(3))).size should be(5)
      }

      "should use Tree.maximum to find the maximum value of the leaves" in {
        Branch(Leaf(1), Branch(Leaf(2), Leaf(3))).maximum should be(3)
      }

      "should use Tree.depth to calculate the maximum depth of a flat tree" in {
        Branch(Leaf(1), Leaf(2)).depth should be(1)
      }

      "should use Tree.depth to calculate the depth of an unbalanced tree" in {
        Branch(Leaf(1), Branch(Leaf(2), Leaf(3))).depth should be(2)
      }

      "should use Tree.map to apply the function to all the elements" in {
        Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))).map(_ * 10) should be(Branch(Leaf(10), Branch(Branch(Leaf(20), Leaf(30)), Leaf(40))))
      }
    }

    "with fold" should {
      import TreeFoldOps._

      "should use Tree.size to calculate the size of the tree" in {
        Branch(Leaf(1), Leaf(2)).size should be(3)
        Branch(Leaf(1), Branch(Leaf(2), Leaf(3))).size should be(5)
      }

      "should use Tree.maximum to find the maximum value of the leaves" in {
        Branch(Leaf(1), Branch(Leaf(2), Leaf(3))).maximum should be(3)
      }

      "should use Tree.depth to calculate the maximum depth of a flat tree" in {
        Branch(Leaf(1), Leaf(2)).depth should be(1)
      }

      "should use Tree.depth to calculate the depth of an unbalanced tree" in {
        Branch(Leaf(1), Branch(Leaf(2), Leaf(3))).depth should be(2)
      }

      "should use Tree.map to apply the function to all the elements" in {
        Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))).map(_ * 10) should be(Branch(Leaf(10), Branch(Branch(Leaf(20), Leaf(30)), Leaf(40))))
      }
    }
  }
}
