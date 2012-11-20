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
import scalaopts.*;
import scalaopts.common.StringUtil;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 */
@SuppressWarnings("unchecked")
public class CommandLineOption<T> {
  public static final int ARITY_UNBOUNDED = scalaopts.package$.MODULE$.UNBOUNDED();

  private final Object lock = new Object();
  private final String name;
  private final Set<String> long_names = new HashSet<String>();
  private final Set<String> short_names = new HashSet<String>();
  private final Set<String> dependencies = new HashSet<String>();
  private boolean required = false;
  private int arity = 1;
  private int min_number_of_required_values = 1;
  private int max_number_of_required_values = 1;
  private String description = StringUtil.empty();
  private IOptionParser<T> option_parser = null;

  private CommandLineOption(String name) {
    this.name = name;
  }

  public static CommandLineOption named(String name) {
    return new CommandLineOption(name);
  }

  public CommandLineOption longName(String alias) {
    synchronized (lock) {
      long_names.add(alias);
    }
    return this;
  }

  public CommandLineOption shortName(String alias) {
    synchronized (lock) {
      short_names.add(alias);
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

  public CommandLineOption required() {
    return required(true);
  }

  public CommandLineOption notRequired() {
    return required(false);
  }

  public CommandLineOption required(boolean required) {
    synchronized (lock) {
      this.required = required;
    }
    return this;
  }

  public CommandLineOption arity(int arity) {
    synchronized (lock) {
      this.arity = arity;
    }
    return this;
  }

  public CommandLineOption minNumberOfRequiredValues(int minNumberOfRequiredValues) {
    synchronized (lock) {
      this.min_number_of_required_values = minNumberOfRequiredValues;
    }
    return this;
  }

  public CommandLineOption maxNumberOfRequiredValues(int maxNumberOfRequiredValues) {
    synchronized (lock) {
      this.max_number_of_required_values = maxNumberOfRequiredValues;
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

  public boolean isRequired() {
    return required;
  }

  public int getArity() {
    return arity;
  }

  public int getMinNumberOfRequiredValues() {
    return min_number_of_required_values;
  }

  public int getMaxNumberOfRequiredValues() {
    return max_number_of_required_values;
  }

  public IOptionParser<T> getOptionParser() {
    return option_parser;
  }

  public Set<String> getLongNames() {
    return Collections.unmodifiableSet(long_names);
  }

  public Set<String> getShortNames() {
    return Collections.unmodifiableSet(short_names);
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
    public boolean isRequired() {
      return CommandLineOption.this.isRequired();
    }

    @Override
    public int getArity() {
      return CommandLineOption.this.getArity();
    }

    @Override
    public int getMinNumberOfRequiredValues() {
      return CommandLineOption.this.getMinNumberOfRequiredValues();
    }

    @Override
    public int getMaxNumberOfRequiredValues() {
      return CommandLineOption.this.getMaxNumberOfRequiredValues();
    }

    @Override
    public Set<String> getLongNames() {
      return CommandLineOption.this.getLongNames();
    }

    @Override
    public Set<String> getShortNames() {
      return CommandLineOption.this.getShortNames();
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
