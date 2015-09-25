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

class IntervalTree[T: ClassTag](initial: List[(Interval[Long], T)]) extends Serializable {
  var root: Node = null

  //Default value
  var chunkSize: Long = 10000
  // TODO: this isnt good syntax
  if (initial != null)
    insert(initial)

  def this() {
    this(null)
  }

  def print() = {
    printNode(root)
  }

  def printNode(n: Node): Unit = {
      println(n.lo, n.hi, n.subtreeMax)
      if (n.leftChild != null) {
        printNode(n.leftChild)
      }
      if (n.rightChild != null) {
        printNode(n.rightChild)
      }
  }

  private def insertRecursive(nodes: List[(Interval[Long], T)]): Unit = {
    if (!nodes.isEmpty) {
      val count = nodes.length
      val middle = count/2
      val node = nodes(middle)

      insert(node)
      insert(nodes.take(middle))
      insert(nodes.drop(middle + 1))
    }
  }

  //Insert Via Block Size
  //Should issue some request to fetch data correctly
  def insert(r: (Interval[Long], T), chunkSize: Long): Boolean  = {
    val length = r._1.end - r._1.start
    if (length < chunkSize) {
      val newPair: (Interval[Long], T) = (new Interval(r._1.start, r._1.start + chunkSize), r._2)
      insert(newPair)
    } else {
      insert(r)
    }
  }

  def insert(r: List[(Interval[Long], T)]) = {

    val nodes = r.sortWith(_._1.start < _._1.start)
    insertRecursive(nodes)

  }

  def insert(r: (Interval[Long], T)): Boolean  = {
    if (root == null) {
      root = new Node(r)
      return true
    }
    var curr: Node = root
    var parent: Node = null
    var search: Boolean = true
    while (search) {
      if (r._1.start < curr.lo) {
        // traverse left subtree
        curr.subtreeMax = Math.max(curr.subtreeMax, r._1.end)
        parent = curr
        curr = curr.leftChild
        if (curr == null) {
          curr = new Node(r)
          parent.leftChild = curr
          search = false
        }
      } else if (r._1.start > curr.lo) {
        // traverse right subtree
        curr.subtreeMax = Math.max(curr.subtreeMax, r._1.end)
        parent = curr
        curr = curr.rightChild
        if (curr == null) {
          curr = new Node(r)
          parent.rightChild= curr
          search = false
        }
      } else {
        // block is a duplicate
        if (r._1.end == curr.hi)
        return false
      }
    }
    false
  }

  def search(r: Interval[Long]): List[T] = {
    search(r, root)
  } 

  private def search(r: Interval[Long], n: Node): List[T] = {
    val results = new ListBuffer[T]()
    if (r.start <= n.hi && r.end >= n.lo) {
      results += n.value
    }
    if (n.subtreeMax < r.start) {
      return results.toList
    }
    if (n.leftChild != null) {
      results ++= search(r, n.leftChild)
    }
    if (n.rightChild != null) {
      results ++= search(r, n.rightChild)
    }
    return results.toList.distinct
  }

  private def rebalance() = {
    // TODO
  }

  class Node(r: (Interval[Long], T)) extends Serializable {
    val lo = r._1.start
    val hi = r._1.end
    var leftChild: Node = null
    var rightChild: Node = null
    var subtreeMax = hi
    val value: T = r._2
  }

}
