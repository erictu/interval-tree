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

class Node[K, T](r: (Interval[Long], T)) {
  val lo = r._1.start
  val hi = r._1.end
  var leftChild: Node[K, T] = null
  var rightChild: Node[K, T] = null
  var subtreeMax = hi
  var dataMap: HashMap[K, T] = new HashMap() 
  var value = r._2
  //TODO: Do we assume everything we insert has data attached?
  // def this(r: Interval[Long]) = {
  //   this((r, T))
  // }

  def greaterThan(r: (Interval[Long], T)): Boolean = {
    return lo > r._1.start 
  }

  def lessThan(r: (Interval[Long], T)): Boolean = {
    return lo < r._1.start 
  }

  //Only used in search, so no data value
  def overlaps(r: Interval[Long]): Boolean = {
    return r.start <= hi && r.end >= lo
  }

  def equals(r: (Interval[Long], T)): Boolean = {
    return r._1.start == lo && r._1.end == hi
  }

  def multiput(rs: List[(K, T)]) = {
    rs.foreach(r => put(r._1, r._2) )
  }

  def put(id: K, data: T) = {
    dataMap += (id -> data)
  }

  //Keep it like this for now until we decide what better representation to return
  def getValue(): T  = {
    value
  }

  def multiget(ids: List[K]): List[T] = {
    var data = new ListBuffer[T]()
    ids.foreach(data += get(_))
    data.toList
  }

  def get(id: K): T = {
    dataMap(id)
  }

}
