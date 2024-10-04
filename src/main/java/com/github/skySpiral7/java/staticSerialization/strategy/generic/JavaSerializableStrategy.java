package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.exception.DeserializationException;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.IntegerSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class JavaSerializableStrategy implements SerializableStrategy
{
   private final EasyReader reader;
   private final EasyAppender appender;
   private final ByteSerializableStrategy byteSerializableStrategy;
   private final IntegerSerializableStrategy integerSerializableStrategy;

   public JavaSerializableStrategy(final EasyReader reader,
                                   final  IntegerSerializableStrategy integerSerializableStrategy)
   {
      this.reader = reader;
      this.appender = null;
      this.byteSerializableStrategy = null;
      this.integerSerializableStrategy = integerSerializableStrategy;
   }

   public JavaSerializableStrategy(final EasyAppender appender,
                                   final ByteSerializableStrategy byteSerializableStrategy)
   {
      this.reader = null;
      this.appender = appender;
      this.byteSerializableStrategy = byteSerializableStrategy;
      this.integerSerializableStrategy = null;
   }

   @Override
   public boolean supportsData(final Class<?> actualClass)
   {
      return Serializable.class.isAssignableFrom(actualClass);
   }

   @Override
   public void write(final Object rawData)
   {
      final Serializable data = (Serializable) rawData;
      final byte[] serializedData = javaSerialize(data);
      byteSerializableStrategy.writeBytes(serializedData.length, 4);
      appender.append(serializedData);
   }

   public static byte[] javaSerialize(final Serializable data)
   {
      //TODO: double check default size after doing some size compares
      final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(512);
      try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
      {
         out.writeObject(data);
      }
      catch (final IOException ex)
      {
         throw new StreamCorruptedException(ex);
      }
      return byteStream.toByteArray();
   }

   @Override
   public <T> T read(final Class<T> actualClass)
   {
      final int length = integerSerializableStrategy.read("Missing java.io.Serializable size");
      final byte[] objectData = StreamCorruptedException.throwIfNotEnoughData(reader, length, "Missing java.io.Serializable data");
      return javaDeserialize(objectData);
   }

   public static <T> T javaDeserialize(final byte[] objectData)
   {
      final ByteArrayInputStream byteStream = new ByteArrayInputStream(objectData);
      try (final ObjectInputStream in = new ObjectInputStream(byteStream))
      {
         return cast(in.readObject());
      }
      catch (final ObjectStreamException ex)
      {
         throw new StreamCorruptedException(ex);
      }
      catch (final ClassNotFoundException | IOException e)
      {
         throw new DeserializationException(e);
      }
   }
}
