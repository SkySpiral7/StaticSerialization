package com.github.skySpiral7.java.staticSerialization.util;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * This utility class is for helping StaticSerializable, not for Serialization in general.
 */
public enum ReflectionUtil
{
   //no instances
   ;

   /**
    * @param subject the class to inspect
    *
    * @return all fields that can and should be written with ObjectStreamWriter.
    * Note that it doesn't consider field type (only modifiers).
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

      final List<Field> allFields = new ArrayList<>();
      allClasses.forEach(clazz -> allFields.addAll(Arrays.asList(clazz.getDeclaredFields())));
      //getDeclaredFields includes private but doesn't include inherited hence the need for allClasses
      //this does include Synthetic but I have no way to exclude
      //java.lang.reflect.Modifier.isSynthetic is not public and $ is now allowed so I can't know
      //but this$0 is final and is the only synthetic field I know of (synthetic classes don't matter)

      //TODO: sort by class then name to allow ensure function and ignore declared order
      return allFields.stream().filter(field -> {
         final int modifiers = field.getModifiers();
         if (Modifier.isFinal(modifiers)) return false;  //can't be read from stream
         if (Modifier.isTransient(modifiers)) return false;  //shouldn't be touched
         if (Modifier.isStatic(modifiers)) return false;  //not related to the instance
         //else attempt to serialize since it could be null or supported
         return true;
      }).collect(Collectors.toList());
   }

}
