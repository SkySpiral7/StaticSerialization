package com.github.skySpiral7.java.staticSerialization.strategy;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;

import com.github.skySpiral7.java.AsynchronousFileAppender;
import com.github.skySpiral7.java.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;

import static com.github.skySpiral7.java.util.ClassUtil.cast;

public enum StringSerializableStrategy
{
   ;  //no instances

   public static void writeWithLength(final AsynchronousFileAppender appender, final String data)
   {
      final byte[] writeMe = data.getBytes(StandardCharsets.UTF_8);
      IntegerSerializableStrategy.write(appender, writeMe.length);
      appender.append(writeMe);
   }

   public static String readWithLength(final AsynchronousFileReader reader)
   {
      final int stringByteLength = IntegerSerializableStrategy.read(reader);
      return cast(reader.readString(stringByteLength));
   }

   public static void writeClassName(final AsynchronousFileAppender appender, final String className)
   {
      //can't use recursion to write the string because that's endless and needs different format
      final byte[] writeMe = className.getBytes(StandardCharsets.UTF_8);
      appender.append(writeMe);
      ByteSerializableStrategy.writeByte(appender, ';');
      //instead of size then string have the string terminated by ; since this saves 3 bytes and class names can't contain ;
   }

   public static String readClassName(final AsynchronousFileReader reader)
   {
      return readClassName(reader, reader.readByte());
   }

   public static String readClassName(final AsynchronousFileReader reader, final byte firstByte)
   {
      final ByteArrayOutputStream classNameStream = new ByteArrayOutputStream();
      classNameStream.write(firstByte);
      while (true)
      {
         if (reader.remainingBytes() == 0) throw new StreamCorruptedException("Incomplete header: class name not terminated");
         final byte thisByte = reader.readByte();
         if (thisByte == ';') break;
         classNameStream.write(thisByte);
      }
      return new String(classNameStream.toByteArray(), StandardCharsets.UTF_8);
   }
}
