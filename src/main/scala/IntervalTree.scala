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

//class IntervalTree[T](allRegions: List[(Interval[Long], T)]) extends Serializable {
class IntervalTree() extends Serializable {

  //var root = new Node(allRegions)
  var root: Node = null

  //def getAllOverlappings(r: Interval[Long]) = allOverlappingRegions(r, root)

  def insert[T](r: (Interval[Long], T)): Boolean  = {
    if (root == null) {
      root = new Node(r)
      true
    }
    var curr: Node[T] = root
    while (curr != null) {
      if (r._1.start < curr.lo) {
        curr.subtreeMax = Math.max(curr.subtreeMax, r._1.end)
        curr = curr.leftChild
      } else {
        curr.subtreeMax = Math.max(curr.subtreeMax, r._1.end)
        curr = curr.rightChild
      }
    }
    curr = new Node(r)
    true
  }

  def insert(record: (Interval[Long], T)) = {
    // Record Fields
    val rStart = record._1.start
    val rEnd = record._1.end
    val rCenter = rStart + (rEnd - rStart) / 2

    // Tree Fields
    var current = root
    var leftMin = root.leftChild.smallestPoint
    var rightMax = root.rightChild.largestPoint
    var center = leftMin + (rightMax - leftMin) / 2

    while (current != null) {
      if (rEnd < center) {
        // Traverse left
        current = current.leftChild
      } else if (rStart > center) {
        // Traverse right
        current = current.rightChild
      } else {
        current.inclusiveIntervals= current.inclusiveIntervals:+record
      }
    }

  }

  class Node[T: ClassTag](r: (Interval[Long], T)) extends Serializable {
    val lo = r._1.start
    val hi = r._1.end
    val leftChild: Node[T] = null
    val rightChild: Node[T] = null
    var subtreeMax = hi
    val value = r._2
  }

  // private def allOverlappingRegions(r: Interval[Long], rt: Node): List[(Interval[Long], T)] = {
  //   if (rt == null) {
  //     return Nil
  //   }
  //   val resultFromThisNode = r match {
  //     case x if (rt.inclusiveIntervals == Nil) => Nil // Sometimes a node can have zero intervals
  //     // Save unnecessary filtering
  //     case x if (x.end < rt.minPointOfCollection || x.start > rt.maxPointOfCollection) => Nil
  //     case _ => rt.inclusiveIntervals.filter(t => r.overlaps(t._1))
  //   }
  //   if (r.overlaps(Interval[Long](rt.centerPoint, rt.centerPoint + 1))) {
  //     return resultFromThisNode ++ allOverlappingRegions(r, rt.leftChild) ++
  //       allOverlappingRegions(r, rt.rightChild)
  //   }
  //   else if (r.end <= rt.centerPoint) {
  //     return resultFromThisNode ++ allOverlappingRegions(r, rt.leftChild)
  //   }
  //   else if (r.start > rt.centerPoint) {
  //     return resultFromThisNode ++ allOverlappingRegions(r, rt.rightChild)
  //   }
  //   else throw new NoSuchElementException("Interval Tree Exception. Illegal " +
  //     "comparison for centerpoint " + rt.centerPoint + " " + r.toString + " cmp: " +
  //     r.overlaps(Interval[Long](rt.centerPoint, rt.centerPoint + 1)))
  // }

  // class Node(allRegions: List[(Interval[Long], T)]) extends Serializable {
  //   private val largestPoint = allRegions.maxBy(_._1.end)._1.end
  //   private val smallestPoint = allRegions.minBy(_._1.start)._1.start
  //   val centerPoint = smallestPoint + (largestPoint - smallestPoint) / 2
  //   val (inclusiveIntervals, leftChild, rightChild) = distributeRegions()
  //   val minPointOfCollection: Long = inclusiveIntervals match {
  //     case Nil => -1
  //     case _ => inclusiveIntervals.minBy(_._1.start)._1.start
  //   }
  //   val maxPointOfCollection: Long = inclusiveIntervals match {
  //     case Nil => -1
  //     case _ => inclusiveIntervals.maxBy(_._1.end)._1.end
  //   }

  //   def distributeRegions() = {
  //     var leftRegions: List[(Interval[Long], T)] = Nil
  //     var rightRegions: List[(Interval[Long], T)] = Nil
  //     var centerRegions: List[(Interval[Long], T)] = Nil
  //     allRegions.foreach(x => {
  //       if (x._1.end < centerPoint) leftRegions ::= x
  //       else if (x._1.start > centerPoint) rightRegions ::= x
  //       else centerRegions ::= x
  //     } )
  //     val leftChild: Node = leftRegions match {
  //       case Nil => null
  //       case _ => new Node(leftRegions)
  //     }
  //     val rightChild: Node = rightRegions match {
  //       case Nil => null
  //       case _ => new Node(rightRegions)
  //     }
  //     (centerRegions, leftChild, rightChild)
  //   }
  // }
}
