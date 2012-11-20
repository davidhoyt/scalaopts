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

package scalaopts.java;

import org.junit.Test;
import scalaopts.DefaultFlagOption;
import scalaopts.DefaultFlagOption$;
import scalaopts.DefaultIntegerOption;
import scalaopts.DefaultIntegerOption$;

import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;

/**
 */
@SuppressWarnings("unchecked")
public class ParserTest {
  @Test
  public void javaTest1() {
    final ICommandLineOption<Integer> option_1 = CommandLineOption
      .named("size")
      .shortName("s")
      .shortName("sz")
      .arity(CommandLineOption.ARITY_UNBOUNDED)
      .describedAs("size description")
      .parseAs(new IOptionTransform<Integer>() {
        @Override
        public Integer apply(String value) {
          return value.length();
        }
      });
    assertEquals(Integer.valueOf(3), option_1.apply("234"));

    final ICommandLineOption<Integer> option_2 = CommandLineOption
      .named("verbose")
      .shortName("v")
      .dependsOn("size")
      .describedAs("verbose description")
      .parseAs(DefaultIntegerOption$.MODULE$);
    assertEquals(Integer.valueOf(234), option_2.apply("234"));
    assertEquals(Integer.valueOf(0), option_2.apply("234a"));

    final IParser parser = CommandLineOptions.build(
        CommandLineOption
            .named("size")
            .required()
            .shortName("s")
            .shortName("sz")
            .describedAs("size description")
            .parseAs(new IOptionTransform<Integer>() {
              @Override
              public Integer apply(String value) {
                return value.length();
              }
            }),

        CommandLineOption
          .named("verbose")
          .shortName("v")
          .dependsOn("size")
          .describedAs("verbose description")
          .parseAs(DefaultFlagOption$.MODULE$)
    );

    parser.parse("-a", "-verbose", "-c");
  }

  @Test
  public void javaTest2() {
    System.out.println("java 2");
    assertTrue(true);
  }
}
