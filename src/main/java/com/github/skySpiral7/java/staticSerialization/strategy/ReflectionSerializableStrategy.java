package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.util.ReflectionUtil;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.lang.reflect.Field;
import java.util.List;

public class ReflectionSerializableStrategy
{
   private static final Logger LOG = LogManager.getLogger();
   private final ObjectStreamReader reader;
   private final ObjectStreamWriter writer;
   private final ReflectionUtil reflectionUtil;

   public ReflectionSerializableStrategy(final ObjectStreamReader reader, final UtilInstances utilInstances)
   {
      this.reader = reader;
      this.writer = null;
      this.reflectionUtil = utilInstances.getReflectionUtil();
   }

   public ReflectionSerializableStrategy(final ObjectStreamWriter writer, final UtilInstances utilInstances)
   {
      this.reader = null;
      this.writer = writer;
      this.reflectionUtil = utilInstances.getReflectionUtil();
   }

   public void write(final Object data)
   {
      final List<Field> allSerializableFields = reflectionUtil.getAllSerializableFields(data.getClass());
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

   public void read(final Object instance)
   {
      final List<Field> allSerializableFields = reflectionUtil.getAllSerializableFields(instance.getClass());
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
