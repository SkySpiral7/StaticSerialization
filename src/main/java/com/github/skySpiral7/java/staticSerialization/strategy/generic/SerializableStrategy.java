package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;

public interface SerializableStrategy
{
   public default boolean supportsHeader(final byte firstByte)
   {
      return false;
   }

   public default Class<?> readHeader(final Class<?> inheritFromClass,
                                      final HeaderSerializableStrategy.PartialHeader partialHeader,
                                      final Class<?> expectedClass,
                                      final boolean allowChildClass)
   {
      //TODO: split into 2 interfaces for header and data
      throw new IllegalStateException("Not implemented");
   }

   public boolean supportsData(final Class<?> actualClass);
   public void write(final Object data);
   public <T> T read(final Class<T> actualClass);
}
