package com.github.skySpiral7.java.staticSerialization;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.List;
import java.util.stream.Collectors;

import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;

/**
 * This utility class is for helping StaticSerializable, not for Serialization in general.
 */
public enum SerializationUtil
{
   ;  //no instances

   /**
    * @param subject the class to inspect
    *
    * @return all fields that can and should be written with ObjectStreamWriter.
    * Known limitation: doesn't support fields of TypeVariable that extend StaticSerializable.
    *
    * @see ClassUtil#getAllFields(Class)
    */
   public static List<Field> getAllSerializableFields(final Class<?> subject)
   {
      final List<Field> allFields = ClassUtil.getAllFields(subject);
      //TODO: sort by class then name to allow ensure function and ignore declared order
      return allFields.stream().filter(field -> {
         final int modifiers = field.getModifiers();
         if (Modifier.isFinal(modifiers)) return false;  //can't be read from stream
         if (Modifier.isTransient(modifiers)) return false;  //shouldn't be touched
         if (Modifier.isStatic(modifiers)) return false;  //not related to the instance
         //TODO: should return true here an attempt the rest

         final Class<?> type = field.getType();
         if (type.isPrimitive()) return true;  //pretty sure type.equals(void.class) isn't possible
         if (StaticSerializable.class.isAssignableFrom(type)) return true;
         if (Serializable.class.isAssignableFrom(type)) return true;  //includes String, enums, boxes, arrays

         return false;
      }).collect(Collectors.toList());
   }
}
