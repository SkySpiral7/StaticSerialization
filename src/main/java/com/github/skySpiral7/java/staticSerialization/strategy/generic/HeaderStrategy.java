package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;

public interface HeaderStrategy
{
   public boolean supportsHeader(final byte firstByte);

   public Class<?> readHeader(final Class<?> inheritFromClass,
                              final HeaderSerializableStrategy.PartialHeader partialHeader,
                              final Class<?> expectedClass,
                              final boolean allowChildClass);
}
