package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.util.ArrayUtil;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;

public class InheritSerializableStrategy implements HeaderStrategy
{
   private final ArrayUtil arrayUtil;
   private final ClassUtil classUtil;
   private final ByteSerializableStrategy byteSerializableStrategy;

   /**
    * For reading
    */
   public InheritSerializableStrategy(final UtilInstances utilInstances)
   {
      this.arrayUtil = utilInstances.getArrayUtil();
      this.classUtil = null;
      this.byteSerializableStrategy = null;
   }

   /**
    * For writing
    */
   public InheritSerializableStrategy(final UtilInstances utilInstances,
                                      final ByteSerializableStrategy byteSerializableStrategy)
   {
      this.arrayUtil = null;
      this.classUtil = utilInstances.getClassUtil();
      this.byteSerializableStrategy = byteSerializableStrategy;
   }

   @Override
   public boolean supportsReadingHeader(final byte firstByte)
   {
      return '?' == firstByte;
   }

   @Override
   public HeaderInformation<?> readHeader(final Class<?> inheritFromClass,
                                          final HeaderInformation.PartialHeader partialHeader,
                                          final Class<?> expectedClass,
                                          final boolean allowChildClass)
   {
      //can't ignore header if inheritFromClass is final because it could be null (thus component will be either '?' or 0xFF)
      final int dimensionCount = arrayUtil.countArrayDimensions(inheritFromClass);
      final Class<?> baseComponent = inheritFromClass.isArray()
         ? arrayUtil.getBaseComponentType(inheritFromClass)
         : inheritFromClass;
      final boolean primitiveArray = baseComponent.isPrimitive();
      return HeaderInformation.forPossibleArray(baseComponent, dimensionCount, primitiveArray);
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
