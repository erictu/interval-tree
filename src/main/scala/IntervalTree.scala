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

package org.apache.intervaltree
import scala.reflect.{ ClassTag, classTag }
import scala.collection.mutable

//class IntervalTree[T](allRegions: List[(Interval[Long], T)]) extends Serializable {
class IntervalTree[T: ClassTag]() extends Serializable {
  var root: Node = null

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
        curr.subtreeMax = Math.max(curr.subtreeMax, r._1.end)
        parent = curr
        curr = curr.leftChild
        if (curr == null) {
          curr = new Node(r)
          parent.leftChild = curr
          search = false
        }
      } else {
        curr.subtreeMax = Math.max(curr.subtreeMax, r._1.end)
        parent = curr
        curr = curr.rightChild
        if (curr == null) {
          curr = new Node(r)
          parent.rightChild= curr
          search = false
        }
      }
    }

    false
  }

  def search(r: Interval[Long]): List[T] = {
    search(r, root).toList
  } 

  private def search(r: Interval[Long], n: Node): mutable.ListBuffer[T] = {

    val results = new mutable.ListBuffer[T]()

    if (r.start <= n.hi && r.end >= n.lo) {
      results += n.value
    }

    if (n.subtreeMax < r.start) {
      return results
    }

    if (n.leftChild != null) {
      results ++= search(r, n.leftChild)
    }
    if (n.rightChild != null) {
      results ++= search(r, n.rightChild)
    }

    // TODO: remove duplicates
    return results
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
