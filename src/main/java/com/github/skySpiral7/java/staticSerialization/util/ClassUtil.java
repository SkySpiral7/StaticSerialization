package com.github.skySpiral7.java.staticSerialization.util;

import java.util.List;

public class ClassUtil
{

   private static final List<?> BOX_CLASS_LIST = List.of(Byte.class, Short.class, Integer.class, Long.class,  //integers
      Float.class, Double.class, Boolean.class, Character.class);

   /**
    * <p>
    * This method will do an unchecked cast to T (the captured return type). The idea is that calling this method allows you to avoid raw
    * types and unchecked casting warnings. Since T will be erased, calling this method won't throw. If the return type of this method is
    * ambiguous, it may take the first choice instead of the desired class. And since overloading is determined at compile time you should
    * double check that you got what you intended.
    * </p>
    * <p>
    * If you have a {@code Class<T> clazz} then instead call clazz.cast which is unchecked and might throw (which is good). clazz.cast also
    * avoids ambiguities mentioned above.
    * </p>
    *
    * @param anything will cast this to T
    * @return anything as T
    */
   @SuppressWarnings("unchecked")
   public static <T> T cast(final Object anything)
   {
      return (T) anything;
   }

   /**
    * @return true if an instance of classInQuestion is a non-void primitive or could be auto-unboxed into one (Void.class and void.class
    * return false). This method is not null safe (throws NPE).
    * @see Class#isPrimitive()
    */
   public boolean isPrimitiveOrBox(final Class<?> classInQuestion)
   {
      //void.class is a primitive but this method ignores that
      final boolean isPrimitive = classInQuestion.isPrimitive() && !void.class.equals(classInQuestion);
      final boolean isBox = BOX_CLASS_LIST.contains(classInQuestion);
      return isPrimitive || isBox;
   }

   /**
    * @return the primitive class that matches the passed in boxedClass
    * @throws IllegalArgumentException if boxedClass isn't a boxed class. Note that java.lang.Void isn't a boxed class.
    */
   public Class<?> unboxClass(final Class<?> boxedClass)
   {
      //boxClassList.contains is pointless: just let it fall through
      if (Byte.class.equals(boxedClass)) return byte.class;
      if (Short.class.equals(boxedClass)) return short.class;
      if (Integer.class.equals(boxedClass)) return int.class;
      if (Long.class.equals(boxedClass)) return long.class;
      if (Float.class.equals(boxedClass)) return float.class;
      if (Double.class.equals(boxedClass)) return double.class;
      if (Boolean.class.equals(boxedClass)) return boolean.class;
      if (Character.class.equals(boxedClass)) return char.class;
      //if(Void.class.equals(boxedClass)) return void.class;
      throw new IllegalArgumentException(boxedClass.getName() + " isn't a box class");
   }

   /**
    * @return the boxed class that matches the passed in primitiveClass
    * @throws IllegalArgumentException if primitiveClass isn't a primitive class
    * @throws IllegalArgumentException if primitiveClass is primitive void.class since java.lang.Void.class isn't a boxed class.
    */
   public Class<?> boxClass(final Class<?> primitiveClass)
   {
      //expectedClass.isPrimitive() is pointless: just let it fall through
      if (byte.class.equals(primitiveClass)) return Byte.class;
      if (short.class.equals(primitiveClass)) return Short.class;
      if (int.class.equals(primitiveClass)) return Integer.class;
      if (long.class.equals(primitiveClass)) return Long.class;
      if (float.class.equals(primitiveClass)) return Float.class;
      if (double.class.equals(primitiveClass)) return Double.class;
      if (boolean.class.equals(primitiveClass)) return Boolean.class;
      if (char.class.equals(primitiveClass)) return Character.class;
      if (void.class.equals(primitiveClass)) throw new IllegalArgumentException("void has no box class");
      throw new IllegalArgumentException(primitiveClass.getName() + " isn't a primitive class");
   }
}
