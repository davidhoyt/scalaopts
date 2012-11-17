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

import scala.Option;
import scalaopts.CustomOptionParser;
import scalaopts.OptionParser;
import scalaopts.common.StringUtil;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 */
@SuppressWarnings("unchecked")
public class CommandLineOption<T> {
  private final Object lock = new Object();
  private final String name;
  private final Set<String> aliases = new HashSet<String>();
  private final Set<String> dependencies = new HashSet<String>();
  private String description = StringUtil.empty();
  private IOptionParser<T> option_parser = null;

  private CommandLineOption(String name) {
    this.name = name;
  }

  public static CommandLineOption named(String name) {
    return new CommandLineOption(name);
  }

  public CommandLineOption alias(String alias) {
    synchronized (lock) {
      aliases.add(alias);
    }
    return this;
  }

  public CommandLineOption dependsOn(String dependency) {
    synchronized (lock) {
      dependencies.add(dependency);
    }
    return this;
  }

  public CommandLineOption describedAs(String description) {
    synchronized (lock) {
      this.description = description;
    }
    return this;
  }

  public ICommandLineOption<T> parseAs(final IOptionTransform<T> optionTransform) {
    return parseAs(new IOptionParser<T>() {
      @Override
      public T getDefaultValue() {
        return null;
      }

      @Override
      public boolean isDefaultValueUsed() {
        return false;
      }

      @Override
      public boolean isAssociatedValueRequired() {
        return true;
      }

      @Override
      public IOptionTransform<T> getTransform() {
        return optionTransform;
      }

      @Override
      public T transform(String value) {
        return optionTransform.apply(value);
      }
    });
  }

  public ICommandLineOption<T> parseAs(final OptionParser<T> optionParser) {
    if (optionParser instanceof CustomOptionParser) {
      return parseAs((CustomOptionParser<T>)optionParser);
    }

    final IOptionTransform<T> optionTransform = new IOptionTransform<T>() {
      @Override
      public T apply(String value) {
        final Option<Object> result = optionParser.apply(value);
        if (result == null || result.isEmpty())
          return (T)null;
        else
          return (T)result.get();
      }
    };

    return parseAs(new IOptionParser<T>() {
      @Override
      public T getDefaultValue() {
        return null;
      }

      @Override
      public boolean isDefaultValueUsed() {
        return false;
      }

      @Override
      public boolean isAssociatedValueRequired() {
        return true;
      }

      @Override
      public IOptionTransform<T> getTransform() {
        return optionTransform;
      }

      @Override
      public T transform(String value) {
        return optionTransform.apply(value);
      }
    });
  }

  public ICommandLineOption<T> parseAs(final CustomOptionParser<T> optionParser) {
    final IOptionTransform<T> optionTransform = new IOptionTransform<T>() {
      @Override
      public T apply(String value) {
        final Option<Object> result = optionParser.apply(value);
        if (result == null || result.isEmpty())
          return (T)null;
        else
          return (T)result.get();
      }
    };

    return parseAs(new IOptionParser<T>() {
      @Override
      public T getDefaultValue() {
        return (T)(optionParser.optionDefaultValue().isEmpty() ? optionParser.optionDefaultValue().get() : null);
      }

      @Override
      public boolean isDefaultValueUsed() {
        return optionParser.useDefaultValue();
      }

      @Override
      public boolean isAssociatedValueRequired() {
        return optionParser.requiresAssociatedValue();
      }

      @Override
      public IOptionTransform<T> getTransform() {
        return optionTransform;
      }

      @Override
      public T transform(String value) {
        return optionTransform.apply(value);
      }
    });
  }

  public ICommandLineOption<T> parseAs(IOptionParser<T> optionParser) {
    synchronized (lock) {
      this.option_parser = optionParser;
      objectBuilt();
    }
    return new Immutable();
  }

  private void objectBuilt() {
    if (!checkIfObjectBuilt()) {
      throw new IllegalStateException("Instance not fully constructed");
    }
  }

  private boolean checkIfObjectBuilt() {
    return (name != null && description != null && option_parser != null);
  }

  public String getName() {
    return name;
  }

  public String getDescription() {
    return description;
  }

  public IOptionParser<T> getOptionParser() {
    return option_parser;
  }

  public Set<String> getAliases() {
    return Collections.unmodifiableSet(aliases);
  }

  public Set<String> getDependencies() {
    return Collections.unmodifiableSet(dependencies);
  }

  public class Immutable implements ICommandLineOption<T> {
    @Override
    public String getName() {
      return CommandLineOption.this.getName();
    }

    @Override
    public String getDescription() {
      return CommandLineOption.this.getDescription();
    }

    @Override
    public Set<String> getAliases() {
      return CommandLineOption.this.getAliases();
    }

    @Override
    public Set<String> getDependencies() {
      return CommandLineOption.this.getDependencies();
    }

    @Override
    public IOptionParser<T> getOptionParser() {
      return CommandLineOption.this.getOptionParser();
    }

    public T apply(String value) {
      return getOptionParser().transform(value);
    }
  }
}
