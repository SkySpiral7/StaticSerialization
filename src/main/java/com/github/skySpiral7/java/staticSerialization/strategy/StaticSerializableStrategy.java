package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.StaticSerializable;
import com.github.skySpiral7.java.staticSerialization.exception.DeserializationException;
import com.github.skySpiral7.java.staticSerialization.exception.InvalidClassException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class StaticSerializableStrategy
{

   public void write(final ObjectStreamWriter writer, final StaticSerializable data)
   {
      data.writeToStream(writer);
   }

   public <T> T read(final ObjectStreamReader reader, final Class<T> expectedClass)
   {
      if (!Modifier.isPublic(expectedClass.getModifiers()))
      {
         throw new InvalidClassException(expectedClass.getName() + " must be public for me to use it");
      }

      final Method method;
      try
      {
         //public static T readFromStream(ObjectStreamReader reader)
         method = expectedClass.getDeclaredMethod("readFromStream", ObjectStreamReader.class);
      }
      catch (final NoSuchMethodException e)
      {
         throw new InvalidClassException(expectedClass.getName() + " implements StaticSerializable but doesn't define readFromStream");
      }

      if (!Modifier.isPublic(method.getModifiers()) || !Modifier.isStatic(method.getModifiers()))
      {
         throw new InvalidClassException(expectedClass.getName() + ".readFromStream must be public static");
      }

      try
      {
         return cast(method.invoke(null, reader));
      }
      catch (final IllegalAccessException | IllegalArgumentException e)
      {
         throw new AssertionError("This can't be thrown", e);
         //since I already know it is public static and I know I'm giving it the right args
         //(because otherwise it wouldn't have been found)
         //TODO: can this be thrown because of modules?
      }
      catch (final InvocationTargetException e)
      {
         throw new DeserializationException(e);
      }
   }
}
