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

import collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import org.bdgenomics.adam.models.ReferenceRegion

class Node[V](r: ReferenceRegion) extends Serializable {
  val region = r
  var leftChild: Node[V] = null
  var rightChild: Node[V] = null
  var subtreeMax = region.end

  // DATA SHOULD BE STORED MORE EFFICIENTLY
  var data: ListBuffer[V] = new ListBuffer()

  def this(i: ReferenceRegion, t: V) = {
    this(i)
    put(t)
  }

  def getSize(): Long = {
    data.length
  }

  override def clone: Node[V] = {
    val n: Node[V] = new Node(region)
    n.data = data
    n
  }

  def clearChildren() = {
    leftChild = null
    rightChild = null
  }

  // TODO: these methods should eventually be moved to ReferenceRegion class
  def greaterThan(other: ReferenceRegion): Boolean = {
    region.referenceName == other.referenceName &&
      region.start > other.start
  }

  def equals(other: ReferenceRegion): Boolean = {
    region.referenceName == other.referenceName &&
      (region.start == other.start && region.end == other.end)
  }

  def lessThan(other: ReferenceRegion): Boolean = {
    region.referenceName == other.referenceName &&
      region.start < other.start
  }

  def overlaps(other: ReferenceRegion): Boolean = {
    region.overlaps(other)
  }

  def multiput(rs: Iterator[V]) = {
    val newData = rs.toList
    data ++= newData
  }

  def put(newData: V) = {
    data += newData
  }

  def get(): Iterator[V] = data.toIterator

}
