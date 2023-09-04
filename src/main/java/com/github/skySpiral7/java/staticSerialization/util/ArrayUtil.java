package com.github.skySpiral7.java.staticSerialization.util;

import java.nio.charset.StandardCharsets;

public enum ArrayUtil
{
   ;  //no instances

   /**
    * @return the number of dimensions of the given array class (max: 255). 0 if not an array class.
    */
   public static int countArrayDimensions(final Class<?> arrayClass)
   {
      final byte[] nameToSearch = arrayClass.getName().getBytes(StandardCharsets.UTF_8);
      int count = 0;
      for (byte currentByte : nameToSearch)
      {
         if (currentByte == '[') ++count;
      }
      return count;
   }

   /**
    * @return the inner most ComponentType of the given array class (may return a primitive class). null if not an array class.
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
