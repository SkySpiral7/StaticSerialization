package com.github.skySpiral7.java.staticSerialization.strategy;

import java.lang.reflect.Field;
import java.util.List;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.util.ReflectionUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public enum ReflectionSerializableStrategy
{
   ;  //no instances
   private static final Logger LOG = LogManager.getLogger();

   public static void write(final ObjectStreamWriter writer, final Object data)
   {
      final List<Field> allSerializableFields = ReflectionUtil.getAllSerializableFields(data.getClass());
      LOG.debug("size: " + allSerializableFields.size());
      allSerializableFields.forEach(field -> {
         field.setAccessible(true);
         try
         {
            final Object fieldValue = field.get(data);
            LOG.debug(field.getDeclaringClass().getName() + "." + field.getName() + ": " + fieldValue);
            writer.writeObject(fieldValue);
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
      final List<Field> allSerializableFields = ReflectionUtil.getAllSerializableFields(instance.getClass());
      LOG.debug("size: " + allSerializableFields.size());
      allSerializableFields.forEach(field -> {
         field.setAccessible(true);
         try
         {
            final Object fieldValue = reader.readObject();
            field.set(instance, fieldValue);  //will auto-cast
            LOG.debug(field.getDeclaringClass().getName() + "." + field.getName() + ": " + fieldValue);
         }
         catch (final IllegalAccessException impossible)
         {
            throw new AssertionError("This can't be thrown.", impossible);
            //since I would've gotten SecurityException from setAccessible(true)
         }
      });
   }
}
