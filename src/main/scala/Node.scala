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

class Node[K, V](r: ReferenceRegion) extends Serializable {
  val region = r
  var leftChild: Node[K, V] = null
  var rightChild: Node[K, V] = null
  var subtreeMax = region.end
  var dataMap: HashMap[K, V] = new HashMap()

  def this(i: ReferenceRegion, k: K, t: V) = {
    this(i)
    put(k, t)
  }

  override def clone: Node[K, V] = {
    val n: Node[K, V] = new Node(region)
    n.dataMap = dataMap
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

  def multiput(rs: Iterator[(K, V)]) = {
    rs.foreach(r => put(r._1, r._2) )
  }

  def put(id: K, data: V) = {
    dataMap += (id -> data)
  }

  def get(id: K): Option[(K,V)] = {
    if (dataMap.contains(id))
      Some((id, dataMap(id)))
    else
      None
  }

  def getAll(): Iterator[(K, V)] = dataMap.toIterator

  def multiget(ids: Iterator[K]): Iterator[(K,V)] = {
    var data = new ListBuffer[(K,V)]()

    ids.foreach(k => {
      val d = get(k)
      if (d.nonEmpty)
        data += d.get
    })
    data.toIterator
  }
}
