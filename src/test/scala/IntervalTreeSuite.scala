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

	test("create intervaltree") {

		var regions = new ListBuffer[(Interval[Long], Long)] 

		for (start <- 1L to 1000L) {
			val end = start + 500L
			val interval = new Interval(start, end)

			val readPair: (Interval[Long], Long) = (interval, start)
			regions += readPair
		}
		val regionsList = regions.toList
		val tree = new IntervalTree(regionsList)
	}

	test("search through intervaltree to get overlapping pairs") {

		val partitions = 10
		var regions = new ListBuffer[(Interval[Long], Long)] 

		for (start <- 1L to 1000L) {
			val end = start + 500L
			val interval = new Interval(start, end)

			val readPair: (Interval[Long], Long) = (interval, start % partitions)
			regions += readPair
		}
		val regionsList = regions.toList
		val tree = new IntervalTree(regionsList)

		val searchInterval = new Interval(250L, 500L)

		val matchedPairs: List[(Interval[Long], Long)] = tree.getAllOverlappings(searchInterval)

		info(matchedPairs.toString)



	}

	test("add new region to intervaltree") {
		assert(0 == 1)
	}

	test("create forest for chromosomes") {
		assert(0 == 1)
	}

}
