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

class IntervalTree[K: ClassTag, T: ClassTag] extends Serializable {
  var root: Node[K, T] = null
  var leftDepth: Long = 0
  var rightDepth: Long = 0

  def IntervalTree() = { 
    root = null
  }

  def IntervalTree(initial: List[(Interval[Long], K, T)]) = {
   // insert(initial)
  }

  def print() = {
    printNode(root)
  }

  def printNode(n: Node[K, T]): Unit = {
      println(n.lo, n.hi, n.subtreeMax)
      println(leftDepth)
      println(rightDepth)
      println(" ")
      if (n.leftChild != null) {
        printNode(n.leftChild)
      }
      if (n.rightChild != null) {
        printNode(n.rightChild)
      }
  }

  // private def insertRecursive(nodes: List[(Interval[Long], Long, T)]): Unit = {
  //   if (!nodes.isEmpty) {
  //     val count = nodes.length
  //     val middle = count/2
  //     val node = nodes(middle)

  //     insert(node)
  //     insert(nodes.take(middle))
  //     insert(nodes.drop(middle + 1))
  //   }
  // }

  // //Insert Via Block Size
  // //Should issue some request to fetch data correctly
  // def insert(r: (Interval[Long], T), chunkSize: Long): Boolean  = {
  //   val length = r._1.end - r._1.start
  //   if (length < chunkSize) {
  //     val newPair: (Interval[Long], T) = (new Interval(r._1.start, r._1.start + chunkSize), r._2)
  //     insert(newPair)
  //   } else {
  //     insert(r)
  //   }
  // }

  // def insert(r: List[(Interval[Long], T)]) = {
  //   val nodes = r.sortWith(_._1.start < _._1.start)
  //   insertRecursive(nodes)
  // }

  // def insert(start: Long, end: Long, data: T): Boolean = {
  //   val interval = new Interval(start, end)
  //   val r = (interval, data)
  //   return insert(r)
  // }

  // def insert(r: (Interval[Long], T)): Boolean  = {
  //   //TODO: make this an option
  //   if (Math.abs(leftDepth - rightDepth) > 4) {
  //     insertHelper(r)
  //     rebalance()
  //     true //recreating entire tree, so return true
  //   } else {
  //     insertHelper(r)
  //   }
  // }

  // def insertHelper(r: (Interval[Long], T)): Boolean  = {
  //   if (root == null) {
  //     root = new Node[K, T](r)
  //     return true
  //   }
  //   var curr: Node[K, T] = root
  //   var parent: Node[K, T] = null
  //   var search: Boolean = true
  //   var leftSide: Boolean = false
  //   var rightSide: Boolean = false
  //   var tempLeftDepth: Long = 0
  //   var tempRightDepth: Long = 0

  //   while (search) {
  //     if (curr.greaterThan(r)) {
  //       // traverse left subtree
  //       if (!leftSide && !rightSide) {
  //         leftSide = true
  //       }
  //       if (rightSide) {
  //         tempRightDepth += 1
  //       } else if (leftSide) {
  //         tempLeftDepth += 1
  //       }
  //       curr.subtreeMax = Math.max(curr.subtreeMax, r._1.end)
  //       parent = curr
  //       curr = curr.leftChild
  //       if (curr == null) {
  //         curr = new Node(r)
  //         parent.leftChild = curr
  //         search = false
  //       }
  //     } else if (curr.lessThan(r)) {
  //       // traverse right subtree
  //       if (!leftSide && !rightSide) {
  //         rightSide = true
  //       }
  //       if (rightSide) {
  //         tempRightDepth += 1
  //       } else if (leftSide) {
  //         tempLeftDepth += 1
  //       }
  //       curr.subtreeMax = Math.max(curr.subtreeMax, r._1.end)
  //       parent = curr
  //       curr = curr.rightChild
  //       if (curr == null) {
  //         curr = new Node(r)
  //         parent.rightChild= curr
  //         search = false
  //       }
  //     } else {
  //       // block is a duplicate
  //       if (curr.equals(r))
  //       return false
  //     }
  //   }
  //   if (tempLeftDepth > leftDepth) {
  //     leftDepth = tempLeftDepth
  //   } else if (tempRightDepth > rightDepth) {
  //     rightDepth = tempRightDepth
  //   }
  //   true
  // }

  /* serches for single interval over single id */
  def search(r: Interval[Long], id: K): List[(K, T)] = {
    search(r, root, Option(List(id)))
  } 

  /* searches for single interval over multiple ids */
  def search(r: Interval[Long], ids: Option[List[K]]): List[(K, T)] = {
    search(r, root, ids)
  } 

  private def search(r: Interval[Long], n: Node[K, T], id: Option[List[K]]): List[(K, T)] = {
    val results = new ListBuffer[(K, T)]()
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
    return results.toList.distinct
  }

  // //currently gets an inorder list of the tree, then bulk constructs a new tree
  // def rebalance() = {
  //   val inOrderedList: List[(Interval[Long], T)] = inOrder(root)
  //   println(inOrderedList)
  //   root = null
  //   insert(inOrderedList)

  // }

  // def inOrder(n: Node[T]): List[(Interval[Long], T)]  = {
  //   val seen = new ListBuffer[(Interval[Long], T)]()
  //   if (n.leftChild != null) {
  //     seen ++= inOrder(n.leftChild)
  //   }
  //   val insertElem: (Interval[Long], T) = (new Interval(n.lo, n.hi), n.value)
  //   seen += insertElem
  //   if (n.rightChild != null) {
  //     seen ++= inOrder(n.rightChild)
  //   }
  //   return seen.toList
  // }

}
