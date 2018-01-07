package com.github.skySpiral7.java.staticSerialization.strategy;

import java.lang.reflect.Field;
import java.util.List;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.SerializationUtil;

public enum ReflectionSerializableStrategy
{
   ;  //no instances

   public static void write(final ObjectStreamWriter writer, final Object data)
   {
      final List<Field> allSerializableFields = SerializationUtil.getAllSerializableFields(data.getClass());
      allSerializableFields.forEach(field -> {
         field.setAccessible(true);
         try
         {
            writer.writeObject(field.get(data));
         }
         catch (final IllegalAccessException impossible)
         {
            throw new AssertionError("This can't be thrown.", impossible);
            //since I would've gotten SecurityException from setAccessible(true)
         }
      });
   }

   public static void read(final ObjectStreamReader reader, final Object instance)
   {
      final List<Field> allSerializableFields = SerializationUtil.getAllSerializableFields(instance.getClass());
      allSerializableFields.forEach(field -> {
         field.setAccessible(true);
         try
         {
            field.set(instance, reader.readObject());  //will auto-cast
         }
         catch (final IllegalAccessException impossible)
         {
            throw new AssertionError("This can't be thrown.", impossible);
            //since I would've gotten SecurityException from setAccessible(true)
         }
      });
   }
}
