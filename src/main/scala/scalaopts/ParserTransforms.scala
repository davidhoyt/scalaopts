/*
  Copyright (C) 2012-2013 the original author or authors.

  See the LICENSE.txt file distributed with this work for additional
  information regarding copyright ownership.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/

package scalaopts

import annotation.tailrec

object ParserTransforms {
  def detectCycle(options: Seq[CommandLineOptionMapTypedValue]): Boolean = {
    type CycleDetectMap = Map[String, (Boolean, List[String])]

    //Create a map...
    val map: CycleDetectMap = options.map(e => e.name -> (false, e.dependencies)).toMap

    def blah0(map: CycleDetectMap, n: String, L: List[String]): (CycleDetectMap, List[String]) = map.get(n) match {
      //if n has not been visited yet then
      case Some((false, deps)) => {
        //mark n as visited
        val updated_map = map.updated(n, (true, deps))

        //for each node m with an edge from n to m do visit(m)
        val (updated_updated_map, updated_L) = deps.foldLeft((updated_map, L))((previous, m) => {
          val (previous_map, previous_L) = previous
          val (next_map, next_L) = blah0(previous_map, m, previous_L)
          (next_map, next_L)
        })

        (updated_updated_map, n :: updated_L)
      }
      case Some((true, _)) => (map, L)
      case _ => (map, L)
    }

    //Perhaps not the most performant. Ideally we could use something like a hashtable for storing the
    //dependency names. And then iterating through all the options and seeing if their name is in the list
    //would be O(n). As it goes, this is potentially O(n^2).
    val all_dependencies = options.foldLeft(List[String]())(_ ++ _.dependencies).distinct
    //Set of all nodes with no incoming edges
    val not_depended_upon = options.filter(opt => !all_dependencies.contains(opt.name)).map(_.name).distinct
    if (!not_depended_upon.isEmpty) {
      val (_, topological_sort) = not_depended_upon.foldLeft((map, List[String]()))((previous, n) => {
        val (previous_map, previous_L) = previous
        val (next_map, next_L) = blah0(previous_map, n, previous_L)
        (next_map, next_L)
      })

      println("TOPO: " + topological_sort)

      topological_sort.size != map.size
    } else {
      true
    }
  }

  def createParserMap(options: Seq[CommandLineOptionMapTypedValue]): CommandLineOptionMap = {
    //Ensure that we have a set of unique names across option names
    val option_names_with_potential_duplicates = options.map(_.name)
    val unique_option_names = option_names_with_potential_duplicates.distinct
    val non_unique_option_names = option_names_with_potential_duplicates.diff(unique_option_names)
    if (!non_unique_option_names.isEmpty) {
      throw new IllegalArgumentException("Command line options must have unique names across the entire set. The following are non-unique names: " + (non_unique_option_names mkString ", "))
    }

    //Ensure that we have a set of unique names across option long names and short names
    val names_with_potential_duplicates = options.map(_.longNames).flatten ++ options.map(_.shortNames).flatten
    val unique_names = names_with_potential_duplicates.distinct
    val non_unique_names = names_with_potential_duplicates.diff(unique_names)
    if (!non_unique_names.isEmpty) {
      throw new IllegalArgumentException("Command line options must have unique long names and short names across the entire set. The following are non-unique names: " + (non_unique_names mkString ", "))
    }

    //Check for invalid dependency graph (cycles on dependencies)
    if (detectCycle(options)) {
      throw new IllegalArgumentException("Cycle detected in the dependencies. You cannot have two dependencies depend on each other either directly or transitively.")
    }

    (
      for {
        opt <- options
      }
        yield opt.name -> (opt, opt.accumulator.initialValue)
    ).toMap
  }

  def createParser(configuration: ParserConfiguration, options: Seq[CommandLineOptionMapTypedValue]): Parser = {
    new Parser(configuration, createParserMap(options))
  }
}
