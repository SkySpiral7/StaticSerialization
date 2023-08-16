package com.github.skySpiral7.java.staticSerialization.util;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public enum ReflectionUtil
{
   //no instances
   ;

   /**
    * @param subject the class to inspect
    * @return all fields that can and should be written with ObjectStreamWriter. Note that it doesn't consider field type (only modifiers).
    */
   public static List<Field> getAllSerializableFields(final Class<?> subject)
   {
      final List<Class<?>> allClasses = new ArrayList<>();
      Class<?> cursor = subject;
      while (cursor != null)
      {
         allClasses.add(cursor);
         cursor = cursor.getSuperclass();
      }
      //allClasses has subject and the extends chain but not interfaces
      //do not look at interfaces since all interface fields are static final

      //getDeclaredFields includes private but doesn't include inherited hence the need for allClasses
      //this does include Synthetic but I have no way to exclude
      //java.lang.reflect.Modifier.isSynthetic is not public and $ is now allowed so I can't know
      //but this$0 is final and is the only synthetic field I know of (synthetic classes don't matter)
      //TODO: is a synthetic class possible? eg annon class. even for non reflection?
      //TODO: what about types like T[] or raw types List<? extends T>

      return allClasses.stream()
         .flatMap(clazz -> Arrays.stream(clazz.getDeclaredFields()))
         .filter(field -> {
            final int modifiers = field.getModifiers();
            if (Modifier.isFinal(modifiers)) return false;  //can't be read from stream
            if (Modifier.isTransient(modifiers)) return false;  //shouldn't be touched
            if (Modifier.isStatic(modifiers)) return false;  //not related to the instance
            //else attempt to serialize since it could be null or supported
            return true;
         })
         //sort by class then name to ensure functionality since getDeclaredFields is officially unordered
         .sorted(Comparator.comparing((Field field) -> field.getDeclaringClass().getName()).thenComparing(Field::getName))
         .collect(Collectors.toList());
   }

}
