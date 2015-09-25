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

	test("insert random regions to intervaltree") {
		val tree = new IntervalTree[Long]()

		val partitions = 10
		var regions = new ListBuffer[(Interval[Long], Long)] 

		val readPair1: (Interval[Long], Long) = (new Interval(1000L, 1999L), 1)
		val readPair2: (Interval[Long], Long) = (new Interval(0L, 999L), 2)
		val readPair3: (Interval[Long], Long) = (new Interval(2000L, 2999L), 3)

		tree.insert(readPair1)
		tree.insert(readPair2)
		tree.insert(readPair3)
		val result: List[Long] = tree.search(new Interval(1000L, 2000L))
		println(result.length)
		assert(result.length == 2)
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
		assert(result.length == 1)
	}

	test("test for multiple overlaps during search") {
		val tree = new IntervalTree[Long]()

		val partitions = 10
		var regions = new ListBuffer[(Interval[Long], Long)] 

		val readPair1: (Interval[Long], Long) = (new Interval(1000L, 2000L), 1)
		val readPair2: (Interval[Long], Long) = (new Interval(0L, 1000L), 2)
		val readPair3: (Interval[Long], Long) = (new Interval(2000L, 3000L), 1)
		val readPair4: (Interval[Long], Long) = (new Interval(2010L, 4000L), 4)
		val readPair5: (Interval[Long], Long) = (new Interval(2100L, 5000L), 3)

		tree.insert(readPair1)
		tree.insert(readPair2)
		tree.insert(readPair3)
		tree.insert(readPair4)
		tree.insert(readPair5)
		val result: List[Long] = tree.search(new Interval(900L, 2110L))
		assert(result.length == 4)
	}

	test("do not insert overlaps in tree") {
		val tree = new IntervalTree[Long]()

		val partitions = 10
		var regions = new ListBuffer[(Interval[Long], Long)] 

		val readPair1: (Interval[Long], Long) = (new Interval(1000L, 1999L), 1)
		val readPair2: (Interval[Long], Long) = (new Interval(0L, 999L), 2)
		val readPair3: (Interval[Long], Long) = (new Interval(0L, 999L), 1)

		tree.insert(readPair1)
		tree.insert(readPair2)
		val inserted = tree.insert(readPair3)
		assert(inserted == false)
	}

	test("insert multiple elements into tree") {
		val tree = new IntervalTree[Long]()

		val readPair1: (Interval[Long], Long) = (new Interval(1000L, 1999L), 1)
		val readPair2: (Interval[Long], Long) = (new Interval(2000L, 2999L), 2)
		val readPair3: (Interval[Long], Long) = (new Interval(0L, 999L), 3)

		val readPairs: List[(Interval[Long], Long)] = List(readPair1, readPair2, readPair3)
		tree.insert(readPairs)

		val searchInterval: Interval[Long] = new Interval(500L, 2040)
		val items = tree.search(searchInterval)
		assert(items.length ==  3)
	}

	// test construct from list of intervals
	test("create a tree from multiple elements") {

		val readPair1: (Interval[Long], Long) = (new Interval(1000L, 1999L), 1)
		val readPair2: (Interval[Long], Long) = (new Interval(2000L, 2999L), 2)
		val readPair3: (Interval[Long], Long) = (new Interval(0L, 999L), 3)

		val readPairs: List[(Interval[Long], Long)] = List(readPair1, readPair2, readPair3)
		val tree = new IntervalTree[Long](readPairs)

		val searchInterval: Interval[Long] = new Interval(500L, 2002)
		val items = tree.search(searchInterval)
		assert(items.length ==  3)
	}

	// test reshaping and squashing
	test("correctly rebalance tree") {
		assert(0 == 1)
	}

	// test reshaping and squashing
	test("correctly squash nodes in tree") {
		assert(0 == 1)
	}
}
