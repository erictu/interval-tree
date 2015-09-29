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

class Node[K, T](r: Interval[Long]) {
  val lo = r.start
  val hi = r.end
  var leftChild: Node[K, T] = null
  var rightChild: Node[T] = null
  var subtreeMax = hi
  var dataMap: HashMap[K, T] = new HashMap() 


  def greaterThan(r: Interval[Long]): Boolean = {
    return lo > r.start 
  }

  def lessThan(r: Interval[Long]): Boolean = {
    return lo < r.start 
  }

  def overlaps(r: Interval[Long]): Boolean = {
    return r.start <= hi && r.end >= lo
  }

  def equals(r: Interval[Long]): Boolean = {
    return r.start == lo && r.end == hi
  }

  def multiput(rs: List[(K, T)]) = {
    rs.foreach(r => put(r._1, r._2) )
  }

  def put(id: Long, data: T) = {
    dataMap += (id -> data)
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
