package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy;

public class NullSerializableStrategy implements HeaderStrategy
{
   private final ByteSerializableStrategy byteSerializableStrategy;

   /**
    * For reading
    */
   public NullSerializableStrategy()
   {
      this.byteSerializableStrategy = null;
   }

   /**
    * For writing
    */
   public NullSerializableStrategy(final ByteSerializableStrategy byteSerializableStrategy)
   {
      this.byteSerializableStrategy = byteSerializableStrategy;
   }

   @Override
   public boolean supportsReadingHeader(final byte firstByte)
   {
      //the empty string class name means null
      return StringSerializableStrategy.TERMINATOR == firstByte;
   }

   @Override
   public HeaderInformation<?> readHeader(final Class<?> inheritFromClass,
                                          final HeaderInformation.PartialHeader partialHeader,
                                          final Class<?> expectedClass,
                                          final boolean allowChildClass)
   {
      return HeaderInformation.forNull();
   }

   @Override
   public boolean supportsWritingHeader(final Class<?> inheritFromClass, final Object data)
   {
      return data == null;
   }

   @Override
   public boolean writeHeader(final Class<?> inheritFromClass, final Object data)
   {
      //if data is null then class name is the empty string
      byteSerializableStrategy.writeByte(StringSerializableStrategy.TERMINATOR);
      return true;
   }
}
