package com.github.skySpiral7.java.staticSerialization.util;

import com.github.skySpiral7.java.util.StringUtil;

public enum ArrayUtil
{
   ;  //no instances

   /**
    * @return the number of dimensions of the given array class (max: 255). 0 if not an array class.
    */
   public static int countArrayDimensions(final Class<?> arrayClass)
   {
      return StringUtil.countCharOccurrences(arrayClass.getName(), '[');
   }

   /**
    * @return the inner most ComponentType of the given array class (may return a primitive class). null if not an array class.
    *
    * @see Class#getComponentType()
    */
   public static Class<?> getBaseComponentType(final Class<?> arrayClass)
   {
      if (!arrayClass.isArray()) return null;
      Class<?> baseComponent = arrayClass.getComponentType();
      while (baseComponent.isArray())
      {
         baseComponent = baseComponent.getComponentType();
      }
      //baseComponent can't be null (by this point) or primitive void (but can be java.lang.Void)
      return baseComponent;
   }
}
