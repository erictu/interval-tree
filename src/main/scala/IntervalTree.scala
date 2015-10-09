/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.akmorrow13.intervaltree
import scala.reflect.{ ClassTag, classTag }
import scala.collection.mutable.ListBuffer

// k = type for entity id, T = value type stored in hashmap 
class IntervalTree[K: ClassTag, T: ClassTag] extends Serializable {
  var root: Node[K, T] = null
  var leftDepth: Long = 0
  var rightDepth: Long = 0
  val threshold = 15
  var nodeCount: Long = 0

  def snapshot(): IntervalTree[K, T] = {
    val newTree: IntervalTree[K,T] = new IntervalTree[K, T]()
    val nodes: List[Node[K, T]] = inOrder()
    newTree.insertRecursive(nodes)
    newTree
  }

  def size(): Long = {
    nodeCount
  }

  def IntervalTree() = { 
    root = null
  }

  def merge(nT: IntervalTree[K,T]): IntervalTree[K,T] = {
    val newNodes: List[Node[K, T]] = nT.inOrder()
    val newTree = this.snapshot()
    newTree.insertRecursive(newNodes)
    newTree
  }

  def printNodes(): Unit = {
    val nodes: List[Node[K, T]] = inOrder().sortWith(_.interval.start < _.interval.start)
    nodes.foreach(r => println(r.interval))
  }

  def insert(i: Interval[Long], r: (K, T)): Boolean  = {
    insert(i, List(r))
  }

  def insert(i: Interval[Long], r: List[(K, T)]): Boolean = {
    insertHelper(i, r)
    if (Math.abs(leftDepth - rightDepth) > threshold) {
      rebalance()
    } 
    true
  }


  private def insertHelper(interval: Interval[Long], r: List[(K, T)]): Boolean  = {
    if (root == null) {
      nodeCount += 1
      root = new Node[K, T](interval)
      root.multiput(r)
      return true
    }
    var curr: Node[K, T] = root
    var parent: Node[K, T] = null
    var search: Boolean = true
    var leftSide: Boolean = false
    var rightSide: Boolean = false
    var tempLeftDepth: Long = 0
    var tempRightDepth: Long = 0

    while (search) {
      if (curr.greaterThan(interval)) {
        // traverse left subtree
        if (!leftSide && !rightSide) {
          leftSide = true
        }
        if (rightSide) {
          tempRightDepth += 1
        } else if (leftSide) {
          tempLeftDepth += 1
        }
        curr.subtreeMax = Math.max(curr.subtreeMax, interval.end)
        parent = curr
        curr = curr.leftChild
        if (curr == null) {
          curr = new Node(interval)
          curr.multiput(r)
          parent.leftChild = curr
          nodeCount += 1
          search = false
        }

      } else if (curr.lessThan(interval)) {
        // traverse right subtree
        if (!leftSide && !rightSide) {
          rightSide = true
        }
        if (rightSide) {
          tempRightDepth += 1
        } else if (leftSide) {
          tempLeftDepth += 1
        }
        curr.subtreeMax = Math.max(curr.subtreeMax, interval.end)
        parent = curr
        curr = curr.rightChild
        if (curr == null) {
          curr = new Node(interval)
          curr.multiput(r)
          parent.rightChild= curr
          nodeCount += 1         
          search = false
        }
      } else {
        // insert new id, given id is not in tree
        curr.multiput(r)
        search = false
      }
    }
    if (tempLeftDepth > leftDepth) {
      leftDepth = tempLeftDepth
    } else if (tempRightDepth > rightDepth) {
      rightDepth = tempRightDepth
    }
    true
  }

  /* serches for single interval over single id */
  def search(r: Interval[Long], id: K): List[(K, T)] = {
    search(r, root, Option(List(id)))
  } 

  /* searches for single interval over multiple ids */
  def search(r: Interval[Long], ids: List[K]): List[(K, T)] = {
    search(r, root, Option(ids))
  } 

  /* searches for single interval over multiple ids */
  def search(r: Interval[Long]): List[(K, T)] = {
    search(r, root, None)
  } 

  private def search(r: Interval[Long], n: Node[K, T], id: Option[List[K]]): List[(K, T)] = {
    val results = new ListBuffer[(K, T)]()
    if (n != null) {
      if (n.overlaps(r)) {
        id match {
          case Some(id) => results ++= n.multiget(id)
          case None     => results ++= n.getAll()
        }
      }
      if (n.subtreeMax < r.start) {
        return results.toList
      }
      if (n.leftChild != null) {
        results ++= search(r, n.leftChild, id)
      }
      if (n.rightChild != null) {
        results ++= search(r, n.rightChild, id)
      }
    }
    return results.toList.distinct
  }

  private def insertNode(n: Node[K, T]): Boolean = {
   if (root == null) {
      root = n
      nodeCount += 1
      return true
    }
    var curr: Node[K, T] = root
    var parent: Node[K, T] = null
    var search: Boolean = true
    var leftSide: Boolean = false
    var rightSide: Boolean = false
    var tempLeftDepth: Long = 0
    var tempRightDepth: Long = 0
    while (search) {
      if (curr.greaterThan(n.interval)) {
        // traverse left subtree
        if (!leftSide && !rightSide) {
          leftSide = true
        }
        if (rightSide) {
          tempRightDepth += 1
        } else if (leftSide) {
          tempLeftDepth += 1
        }
        curr.subtreeMax = Math.max(curr.subtreeMax, n.interval.end)
        parent = curr
        curr = curr.leftChild
        if (curr == null) {
          parent.leftChild = n
          nodeCount += 1
          search = false
        }

      } else if (curr.lessThan(n.interval)) {
        // traverse right subtree
        if (!leftSide && !rightSide) {
          rightSide = true
        }
        if (rightSide) {
          tempRightDepth += 1
        } else if (leftSide) {
          tempLeftDepth += 1
        }
        curr.subtreeMax = Math.max(curr.subtreeMax, n.interval.end)
        parent = curr
        curr = curr.rightChild
        if (curr == null) {
          parent.rightChild= n
          nodeCount += 1         
          search = false
        }
      } else {
        // attempting to replace a nodealready in tree. Merge
        curr.multiput(n.getAll())
        search = false
      }
    }
    if (tempLeftDepth > leftDepth) {
      leftDepth = tempLeftDepth
    } else if (tempRightDepth > rightDepth) {
      rightDepth = tempRightDepth
    }
    true
  }

  private def insertRecursive(nodes: List[Node[K,T]]): Unit = {
    if (nodes == null) {
      return
    }
    if (!nodes.isEmpty) {
      val count = nodes.length
      val middle = count/2
      val node = nodes(middle)

      insertNode(node)
      insertRecursive(nodes.take(middle))
      insertRecursive(nodes.drop(middle + 1))
    }
  }

  // currently gets an inorder list of the tree, then bulk constructs a new tree
  private def rebalance() = {
    val nodes: List[Node[K,T]] = inOrder()
    root = null
    nodeCount = 0
    val orderedList = nodes.sortWith(_.interval.start < _.interval.start)
    orderedList.foreach(n => n.clearChildren())
    insertRecursive(orderedList)
  }

  private def inOrder(): List[Node[K, T]] = {
    return inOrder(root).toList
  }

  private def inOrder(n: Node[K, T]): ListBuffer[Node[K, T]] = {
    val seen = new ListBuffer[Node[K, T]]()
    if (n == null) {
      return seen
    }
    seen += n.clone

    seen ++= inOrder(n.leftChild)
    seen ++= inOrder(n.rightChild)
    seen
  }

}
