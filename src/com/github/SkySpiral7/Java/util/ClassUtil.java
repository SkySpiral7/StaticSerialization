package com.github.SkySpiral7.Java.util;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public enum ClassUtil
{
   ;  //no instances

   /**
    * <p>
    * This method will do an unchecked cast to T (the captured return type). The idea is that calling this method allows
    * you to avoid raw types and unchecked casting warnings. Since T will be erased, calling this method won't throw. If
    * the return type of this method is ambiguous, it may take the first choice instead of the desired class. And since
    * overloading is determined at compile time you should double check that you got what you intended.
    * </p>
    * <p>
    * If you have a {@code Class<T> clazz} then instead call clazz.cast which is unchecked and might throw (which is
    * good). clazz.cast also avoids ambiguities mentioned above.
    * </p>
    *
    * @param anything
    *       will cast this to T
    * @return anything as T
    */
   @SuppressWarnings("unchecked")
   public static <T> T cast(final Object anything)
   {
      return (T) anything;
   }

   /**
    * This method returns the List of all Fields for a class.
    * All fields are included regardless of scope (eg private, public),
    * and regardless of modifiers (eg static, transient).
    * All fields are included in the base class and all parent class,
    * however enclosing classes are excluding because such Fields can't be used directly on an object of class anyClass.
    * Additionally generated fields are excluded because you usually don't want to touch them.
    *
    * @return a List of every possible Field that can be used by any object of the given anyClass
    * except generated ones
    */
   public static List<Field> getAllFields(final Class<?> anyClass)  //TODO: either include interface fields or exclude static
   {
      final List<Class<?>> allClasses = new ArrayList<>();
      Class<?> cursor = anyClass;
      while (cursor != null)
      {
         allClasses.add(cursor);
         cursor = cursor.getSuperclass();
      }

      final List<Field> result = new ArrayList<>();
      allClasses.forEach(clazz -> result.addAll(Arrays.asList(clazz.getDeclaredFields())));
      return result.stream().filter(field ->
      {
         //exclude generated fields
         return !field.getName().contains("$");
      }).collect(Collectors.toList());
   }

   /**
    * @return true if an instance of classInQuestion could be auto-unboxed (excludes Void.class).
    *
    * @see Class#isPrimitive()
    */
   public static boolean isBoxedPrimitive(final Class<?> classInQuestion)
   {
      return Arrays.asList(Byte.class, Short.class, Integer.class, Long.class,  //integers
            Float.class, Double.class, Boolean.class, Character.class).contains(classInQuestion);
   }

   /**
    * @return the primitive class that matches the passed in boxedClass
    *
    * @throws IllegalArgumentException
    *       if boxedClass isn't a boxed class. Note that Void isn't a boxed class.
    */
   public static Class<?> unboxClass(final Class<?> boxedClass)
   {
      //isBoxedPrimitive(boxedClass) is pointless: just let it fall through
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
    *
    * @throws IllegalArgumentException
    *       if primitiveClass isn't a primitive class or is void.class since Void.class isn't a boxed class.
    */
   public static Class<?> boxClass(final Class<?> primitiveClass)
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
      //if (void.class.equals(primitiveClass)) return Void.class;
      throw new IllegalArgumentException(primitiveClass.getName() + " isn't a primitive class or is void.class");
   }
}
