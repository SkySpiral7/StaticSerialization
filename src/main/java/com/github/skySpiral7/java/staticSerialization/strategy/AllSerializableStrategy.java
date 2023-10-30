package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.ArraySerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.BoxPrimitiveSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.EnumSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.JavaSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.SerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StaticSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StringSerializableStrategy;

import java.util.List;

public class AllSerializableStrategy
{
   private final List<SerializableStrategy> strategyList;

   public AllSerializableStrategy(final ArraySerializableStrategy arraySerializableStrategy,
                                  final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy,
                                  final EnumSerializableStrategy enumSerializableStrategy,
                                  final JavaSerializableStrategy javaSerializableStrategy,
                                  final StaticSerializableStrategy staticSerializableStrategy,
                                  final StringSerializableStrategy stringSerializableStrategy)
   {
      /* order: supported java.lang serial must be before javaSerializableStrategy so that they get compression,
       * staticSerializableStrategy before java since it has priority,
       * enum in between so that it can be static by default but can also be manually serial. */
      strategyList = List.of(boxPrimitiveSerializableStrategy, stringSerializableStrategy, arraySerializableStrategy,
         staticSerializableStrategy, enumSerializableStrategy, javaSerializableStrategy);
   }

   public void write(final Object data)
   {
      final Class<?> dataClass = data.getClass();
      strategyList.stream()
         .filter(strategy -> strategy.supports(dataClass))
         .findFirst()
         .ifPresentOrElse(strategy -> strategy.write(data), () -> {throw new NotSerializableException(dataClass);});
   }

   public <T> T read(final Class<T> actualClass)
   {
      return strategyList.stream()
         .filter(strategy -> strategy.supports(actualClass))
         .findFirst()
         .orElseThrow(() -> new NotSerializableException(actualClass))
         .read(actualClass);
   }
}
