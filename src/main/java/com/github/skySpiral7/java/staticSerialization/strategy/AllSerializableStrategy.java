package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.ArraySerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.BitSetSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.BoxPrimitiveSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.ClassHeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.EnumSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.JavaSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.SerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StaticSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StringSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.UuidSerializableStrategy;

import java.util.List;

public class AllSerializableStrategy
{
   private final List<SerializableStrategy> strategyList;

   public AllSerializableStrategy(final ArraySerializableStrategy arraySerializableStrategy,
                                  final BitSetSerializableStrategy bitSetSerializableStrategy,
                                  final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy,
                                  final ClassHeaderSerializableStrategy classHeaderSerializableStrategy,
                                  final EnumSerializableStrategy enumSerializableStrategy,
                                  final JavaSerializableStrategy javaSerializableStrategy,
                                  final StaticSerializableStrategy staticSerializableStrategy,
                                  final StringSerializableStrategy stringSerializableStrategy,
                                  final UuidSerializableStrategy uuidSerializableStrategy)
   {
      /* order:
       * first is supported jdk final classes (none of which are static) so that they have better compression than java.
       * then static so that it will respect any manual serial.
       * then bitset/enum (which can be static) so that the non-static ones will have better compression than java.
       * then java if all else fails */
      strategyList = List.of(
         boxPrimitiveSerializableStrategy, stringSerializableStrategy, arraySerializableStrategy,
         uuidSerializableStrategy,
         staticSerializableStrategy,
         bitSetSerializableStrategy, enumSerializableStrategy,
         javaSerializableStrategy,
         classHeaderSerializableStrategy);
      /*
       * TODO: also Big int/dec, stream: new ArrayList<>().stream().collect(Collectors.toList()).toArray()
       * there's already enum. big int/dec could do same
       * only auto do it if it is always more compressed and accounts for all data
       * big int -> byte[]
       * big dec -> toEngineeringString? can't see any way to get base big int
       */
   }

   public Class<?> readHeader(final Class<?> inheritFromClass,
                              final HeaderSerializableStrategy.PartialHeader partialHeader,
                              final Class<?> expectedClass,
                              final boolean allowChildClass)
   {
      return strategyList.stream()
         .filter(strategy -> strategy.supportsHeader(partialHeader.firstByte()))
         .findFirst()
         .map(strategy -> strategy.readHeader(inheritFromClass, partialHeader, expectedClass, allowChildClass))
         .orElse(null);
   }

   public void write(final Object data)
   {
      final Class<?> dataClass = data.getClass();
      strategyList.stream()
         .filter(strategy -> strategy.supportsData(dataClass))
         .findFirst()
         .orElseThrow(() -> new NotSerializableException(dataClass))
         .write(data);
   }

   public <T> T read(final Class<T> actualClass)
   {
      return strategyList.stream()
         .filter(strategy -> strategy.supportsData(actualClass))
         .findFirst()
         .orElseThrow(() -> new NotSerializableException(actualClass))
         .read(actualClass);
   }
}
