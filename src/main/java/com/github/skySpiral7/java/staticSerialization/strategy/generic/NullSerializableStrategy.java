package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;

public class NullSerializableStrategy implements HeaderStrategy
{
   @Override
   public boolean supportsHeader(final byte firstByte)
   {
      //the empty string class name means null
      return StringSerializableStrategy.TERMINATOR == firstByte;
   }

   @Override
   public HeaderInformation<?> readHeader(final Class<?> inheritFromClass,
                                          final HeaderSerializableStrategy.PartialHeader partialHeader,
                                          final Class<?> expectedClass,
                                          final boolean allowChildClass)
   {
      return HeaderInformation.forNull(partialHeader.firstByte());
   }
}
