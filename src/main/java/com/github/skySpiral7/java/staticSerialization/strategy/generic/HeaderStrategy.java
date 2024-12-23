package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;

public interface HeaderStrategy
{
   public boolean supportsReadingHeader(final byte firstByte);

   public HeaderInformation<?> readHeader(final Class<?> inheritFromClass,
                                          final HeaderInformation.PartialHeader partialHeader,
                                          final Class<?> expectedClass,
                                          final boolean allowChildClass);

   public boolean supportsWritingHeader(final Class<?> inheritFromClass, final Object data);

   /**
    * @return true if the data was fully represented by a header and thus no more data should be written
    */
   public boolean writeHeader(final Class<?> inheritFromClass, final Object data);
}
