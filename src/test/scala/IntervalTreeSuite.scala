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
import org.bdgenomics.adam.models.ReferenceRegion

class IntervalTreeSuite extends FunSuite {

	test("insert regions to intervaltree for only one entity") {
		val tree = new IntervalTree[Long, Long]()

		val partitions = 10

		val id = 1L
		for (start <- 1L to 6L) {
			val end = start + 500L

			val region = new ReferenceRegion("region", start, end)
			val partition: Long = start % partitions
			tree.insert(region, (id, partition))
		}
		assert(tree.size() == 6)
	}

	test("insert different ids into same node, tests search all and search 1") {
		val tree = new IntervalTree[Long, Long]()

		val partitions = 10

		val start = 0L
		val end = 1000L
		val region = new ReferenceRegion("region", start, end)

		for (id <- 1L to 6L) {
			val partition: Long = start % partitions
			tree.insert(region, (id, partition))
		}

		var searchOne: List[(Long, Long)] = tree.search(region, 1L)
		assert(searchOne.size == 1)

		// search for all
		var searchAll: List[(Long, Long)] = tree.search(region)
		assert(searchAll.size == 6)

		assert(tree.size() == 1)		
	}

	test("search for only some ids") {
		val tree = new IntervalTree[Long, Long]()

		val start = 0L
		val end = 1000L
		val region = new ReferenceRegion("region", start, end)

		for (id <- 1L to 6L) {
			val partition: Long = id
			tree.insert(region, (id, partition))
		}

		// create multiple ids to be searched
		val ids: List[Long] = List(1L, 3L, 5L)
		val searchSome: List[(Long, Long)] = tree.search(region, ids)	
		assert(searchSome.contains((1L, 1L)) == true)
		assert(searchSome.contains((3L, 3L)) == true)
		assert(searchSome.contains((3L, 3L)) == true)

	}

	test("insert ids in bulk with same interval") {
		val tree = new IntervalTree[Long, Long]()

		val r: List[(Long, Long)] = List((1L, 2L), (3L, 4L), (5L, 6L))

		val region = new ReferenceRegion("region", 0, 1000)

		tree.insert(region, r)

		assert(tree.size == 1)
		assert(tree.search(region).size == 3)

	}

	test("rebalancing tree") {
		val tree = new IntervalTree[Long, Long]()

		for (id <- 1L to 50L) {
			val partition: Long = id
			val region = new ReferenceRegion("region", id + 7L, id + 1000L)
			tree.insert(region, (id, partition))
		}

		assert(tree.rightDepth - tree.leftDepth <= 16)
		assert(tree.size == 50)

	}

	test("clone tree") {
		val tree = new IntervalTree[Long, Long]()

		for (id <- 1L to 50L) {
			val partition: Long = id
			val region = new ReferenceRegion("region", id + 7L, id + 1000L)
			tree.insert(region, (id, partition))
		}

		val newTree = tree.snapshot()
		assert(tree.size() == newTree.size())
	}

	test("merge 2 trees with nonoverlapping intervals") {
		var totalRecs = 0
		val tree1 = new IntervalTree[Long, Long]()

		for (id <- 1L to 10L) {
			val partition: Long = id
			val region = new ReferenceRegion("region", id , id + 1000L)
			tree1.insert(region, (id, partition))
			totalRecs += 1
		}

		val tree2 = new IntervalTree[Long, Long]()

		for (id <- 11L to 20L) {
			val partition: Long = id
			val region = new ReferenceRegion("region", id , id + 1000L)			
			tree2.insert(region, (id, partition))
			totalRecs += 1
		}

		val newTree = tree1.merge(tree2)
		assert(newTree.size == totalRecs)
	}		

	// test edge cases
	test("take snapshot of empty tree") {
		val tree = new IntervalTree[Long, Long]()
		val newTree = tree.snapshot()
	}

	test("search empty tree") {
		val tree = new IntervalTree[Long, Long]()

		// create interval to search
		val start = 0L
		val end = 1000L
		val region = new ReferenceRegion("region", start, end)

		val ids: List[Long] = List(1L, 3L, 5L)
		tree.search(region, ids)	
	}

	test("difference between insertRegion and insertNode: RefRegion is the same") {
		val tree1 = new IntervalTree[Long, Long]()
		val tree2 = new IntervalTree[Long, Long]()
		val partitions = 10

		val start = 0L
		val end = 1000L
		val region = new ReferenceRegion("region", start, end)
		//all the data should go into just one node for regular insert
		//but we should be left with 6 nodes for insertNode
		for (id <- 1L to 6L) {
			val partition: Long = start % partitions
			tree1.insert(region, (id, partition))
			val newNode = new Node[Long, Long](region)
			newNode.put(id, partition)
			tree2.insertNode(newNode)
		}

		println("regular insert method tree size: " + tree1.size())
		println("insertNode method tree size: " + tree2.size())
		tree1.printNodes()
		tree2.printNodes()
		assert(tree1.size() == 1)
		assert(tree2.size() == 1) //should be the same because the data is merged with the same region
	}

	test("difference between insertRegion and insertNode: RefRegion is the different") {
		val tree1 = new IntervalTree[Long, Long]()
		val tree2 = new IntervalTree[Long, Long]()

		//regions is smaller and smaller subsets
		val reg1 = new ReferenceRegion("region", 0L, 1000L)
		val reg2 = new ReferenceRegion("region", 100L, 900L)
		val reg3 = new ReferenceRegion("region", 300L, 700L)

		//(region, (id, partitionNum))
		tree1.insert(reg1, (1L, 1L))
		val node1 = new Node[Long, Long](reg1)
		node1.put(1L, 1L)
		tree2.insertNode(node1)

		tree1.insert(reg1, (2L, 2L))
		val node2 = new Node[Long, Long](reg2)
		node2.put(2L, 2L)
		tree2.insertNode(node2)

		tree1.insert(reg1, (3L, 3L))
		val node3 = new Node[Long, Long](reg3)
		node3.put(3L, 3L)
		tree2.insertNode(node3)

		println("regular insert method tree size: " + tree1.size())
		println("insertNode method tree size: " + tree2.size())
		tree1.printNodes()
		tree2.printNodes()
		assert(tree1.size() == 1)
		assert(tree2.size() == 3)	
	}
}
