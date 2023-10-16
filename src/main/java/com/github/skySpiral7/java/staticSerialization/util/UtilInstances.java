package com.github.skySpiral7.java.staticSerialization.util;

/**
 * Holds a reference to every strategy object. This is a lazy way to implement DI independent of client DI.
 */
public class UtilInstances
{
   private final ArrayUtil arrayUtil;
   private final BitWiseUtil bitWiseUtil;
   private final ClassUtil classUtil;
   private final ReflectionUtil reflectionUtil;

   public UtilInstances()
   {
      arrayUtil = new ArrayUtil();
      bitWiseUtil = new BitWiseUtil();
      //TODO: do other utils
      classUtil = null;
      reflectionUtil = null;
   }

   public ArrayUtil getArrayUtil()
   {
      return arrayUtil;
   }

   public BitWiseUtil getBitWiseUtil()
   {
      return bitWiseUtil;
   }

   public ClassUtil getClassUtil()
   {
      return classUtil;
   }

   public ReflectionUtil getReflectionUtil()
   {
      return reflectionUtil;
   }
}
