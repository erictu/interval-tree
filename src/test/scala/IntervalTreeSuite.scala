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

import org.scalatest.FunSuite
import org.scalatest.Matchers
import scala.collection.mutable.ListBuffer

class IntervalTreeSuite extends FunSuite {

	test("insert region to intervaltree") {
		val tree = new IntervalTree[Long]()

		val partitions = 10
		var regions = new ListBuffer[(Interval[Long], Long)] 

		for (start <- 1L to 6L) {
			val end = start + 500L
			val interval = new Interval(start, end)
			val partition: Long = start % partitions
			val readPair: (Interval[Long], Long) = (interval, partition)
			tree.insert(readPair)
		}
	}

	test("insert ranodm regions to intervaltree") {
		val tree = new IntervalTree[Long]()

		val partitions = 10
		var regions = new ListBuffer[(Interval[Long], Long)] 

		val readPair1: (Interval[Long], Long) = (new Interval(1000L, 2000L), 1)
		val readPair2: (Interval[Long], Long) = (new Interval(0L, 1000L), 2)
		val readPair3: (Interval[Long], Long) = (new Interval(2000L, 3000L), 3)

		tree.insert(readPair1)
		tree.insert(readPair2)
		tree.insert(readPair3)
		val result: List[Long] = tree.search(new Interval(1000L, 2000L))
		println(result.length)
		assert(result.length == 3)
	}

	test("search for interval with no overlaps") {
		val tree = new IntervalTree[Long]()

		val partitions = 10
		var regions = new ListBuffer[(Interval[Long], Long)] 

		val readPair1: (Interval[Long], Long) = (new Interval(1000L, 2000L), 1)
		val readPair2: (Interval[Long], Long) = (new Interval(0L, 1000L), 2)
		val readPair3: (Interval[Long], Long) = (new Interval(2000L, 3000L), 3)

		tree.insert(readPair1)
		tree.insert(readPair2)
		tree.insert(readPair3)
		val result: List[Long] = tree.search(new Interval(5000L, 7000L))
		println(result.length)
		assert(result.length == 0)
	}

	test("test for overlap removal during search") {
		val tree = new IntervalTree[Long]()

		val partitions = 10
		var regions = new ListBuffer[(Interval[Long], Long)] 

		val readPair1: (Interval[Long], Long) = (new Interval(1000L, 2000L), 1)
		val readPair2: (Interval[Long], Long) = (new Interval(0L, 1000L), 1)
		val readPair3: (Interval[Long], Long) = (new Interval(2000L, 3000L), 1)

		tree.insert(readPair1)
		tree.insert(readPair2)
		tree.insert(readPair3)
		val result: List[Long] = tree.search(new Interval(1500L, 2010L))
		println(result.length)
		assert(result.length == 1)
	}

}
