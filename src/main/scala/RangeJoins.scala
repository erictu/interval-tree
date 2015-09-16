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

/*intervals with different groupId values do not overlap*/
case class Interval[T <% Long](start: T, end: T, groupId: Option[String]=None) {
  def overlaps(other: Interval[T]): Boolean = {
    (groupId == other.groupId) &&
    (end >= start) && (other.end >= other.start) &&
      (end > other.start && start < other.end)
  }
}
