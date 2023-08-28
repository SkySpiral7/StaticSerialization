package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.DeserializationException;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;

import static com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy.writeBytes;
import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public enum JavaSerializableStrategy
{
   ;  //no instances

   public static void writeWithLength(final EasyAppender appender, final Serializable data)
   {
      final byte[] serializedData = javaSerialize(data);
      writeBytes(appender, serializedData.length, 4);
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

   public static <T> T readWithLength(final EasyReader reader)
   {
      final int length = IntegerSerializableStrategy.read(reader, "Missing java.io.Serializable size");
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
