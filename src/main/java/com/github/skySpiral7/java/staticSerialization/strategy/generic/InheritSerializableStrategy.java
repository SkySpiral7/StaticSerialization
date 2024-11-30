package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;

public class InheritSerializableStrategy implements HeaderStrategy
{
   private final ClassUtil classUtil;
   private final ByteSerializableStrategy byteSerializableStrategy;

   public InheritSerializableStrategy(final ClassUtil classUtil,
                                      final ByteSerializableStrategy byteSerializableStrategy)
   {
      this.classUtil = classUtil;
      this.byteSerializableStrategy = byteSerializableStrategy;
   }

   @Override
   public boolean supportsReadingHeader(final byte firstByte)
   {
      return false;
   }

   @Override
   public HeaderInformation<?> readHeader(final Class<?> inheritFromClass,
                                          final HeaderSerializableStrategy.PartialHeader partialHeader,
                                          final Class<?> expectedClass,
                                          final boolean allowChildClass)
   {
      throw new IllegalStateException("Should not be called");
   }

   @Override
   public boolean supportsWritingHeader(final Class<?> inheritFromClass, final Object data)
   {
      if (null == inheritFromClass) return false;
      final Class<?> dataClass = data.getClass();
      if (Boolean.class.equals(dataClass)) return false;

      if (inheritFromClass.isPrimitive())
      {
         return classUtil.boxClass(inheritFromClass).equals(dataClass);
      }
      return inheritFromClass.equals(dataClass);
   }

   @Override
   public boolean writeHeader(final Class<?> inheritFromClass, final Object data)
   {
      //inherit primitive array has no header but has data
      if (inheritFromClass.isPrimitive()) return false;

      byteSerializableStrategy.writeByte('?');
      return false;
   }
}
