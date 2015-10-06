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

	test("insert regions to intervaltree for only one entity") {
		val tree = new IntervalTree[Long, Long]()

		val partitions = 10

		val id = 1L
		for (start <- 1L to 6L) {
			val end = start + 500L

			val interval = new Interval(start, end)
			val partition: Long = start % partitions
			tree.insert(interval, (id, partition))
		}
		assert(tree.size() == 6)
	}

	test("insert different ids into same node, tests search all and search 1") {
		val tree = new IntervalTree[Long, Long]()

		val partitions = 10

		val start = 0L
		val end = 1000L
		val interval = new Interval(start, end)

		for (id <- 1L to 6L) {
			val partition: Long = start % partitions
			tree.insert(interval, (id, partition))
		}

		var searchOne: List[(Long, Long)] = tree.search(interval, 1L)
		assert(searchOne.size == 1)

		// search for all
		var searchAll: List[(Long, Long)] = tree.search(interval)
		assert(searchAll.size == 6)

		assert(tree.size() == 1)		
	}

	test("search for only some ids") {
		val tree = new IntervalTree[Long, Long]()

		val start = 0L
		val end = 1000L
		val interval = new Interval(start, end)

		for (id <- 1L to 6L) {
			val partition: Long = id
			tree.insert(interval, (id, partition))
		}

		// create multiple ids to be searched
		val ids: List[Long] = List(1L, 3L, 5L)
		val searchSome: List[(Long, Long)] = tree.search(interval, ids)	
		assert(searchSome.contains((1L, 1L)) == true)
		assert(searchSome.contains((3L, 3L)) == true)
		assert(searchSome.contains((3L, 3L)) == true)

	}

	test("insert ids in bulk with same interval") {
		val tree = new IntervalTree[Long, Long]()

		val r: List[(Long, Long)] = List((1L, 2L), (3L, 4L), (5L, 6L))

		val i: Interval[Long] = new Interval(0, 1000)

		tree.insert(i, r)

		assert(tree.size == 1)
		assert(tree.search(i).size == 3)

	}

	test("rebalancing tree") {
		val tree = new IntervalTree[Long, Long]()

		for (id <- 1L to 50L) {
			val partition: Long = id
			val interval = new Interval(id + 7L, id + 1000L)
			tree.insert(interval, (id, partition))
		}

		assert(tree.rightDepth - tree.leftDepth <= 5)

	}

	test("clone tree") {
		val tree = new IntervalTree[Long, Long]()

		for (id <- 1L to 50L) {
			val partition: Long = id
			val interval = new Interval(id + 7L, id + 1000L)
			tree.insert(interval, (id, partition))
		}

		val newTree = tree.snapshot()
		assert(tree.size() == newTree.size())
	}

	test("take snapshot of empty tree") {
		val tree = new IntervalTree[Long, Long]()
		val newTree = tree.snapshot()
	}

	test("search empty tree") {
		val tree = new IntervalTree[Long, Long]()

		// create interval to search
		val start = 0L
		val end = 1000L
		val interval = new Interval(start, end)

		val ids: List[Long] = List(1L, 3L, 5L)
		tree.search(interval, ids)	
	}
}
