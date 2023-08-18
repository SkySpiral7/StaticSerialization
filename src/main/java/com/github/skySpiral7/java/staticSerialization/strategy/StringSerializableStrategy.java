package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;

public enum StringSerializableStrategy
{
   ;  //no instances
   private static final Logger LOG = LogManager.getLogger();

   //TODO: maybe use DI instead of static all. more garbage but allows test mocking
   //could have new ObjectStreamWriter/Reader do new AllDependencies() which new() them all and passes in bundle
   public static void writeWithLength(final EasyAppender appender, final String data)
   {
      LOG.debug(data);
      final byte[] writeMe = data.getBytes(StandardCharsets.UTF_8);
      IntegerSerializableStrategy.write(appender, writeMe.length);
      appender.append(writeMe);
   }

   public static String readWithLength(final EasyReader reader)
   {
      final int stringByteLength = IntegerSerializableStrategy.read(reader);
      /*TODO: could use an Overlong null delimiter 0xC080 to reduce overhead by 2 but harder to read stream
      could also make a new string type for null delimited 0x00 (only used when contains no null)
      AsynchronousFileReader would need a readBytesUntil*/
      final String result = new String(reader.readBytes(stringByteLength), StandardCharsets.UTF_8);
      LOG.debug(result);
      return result;
   }

   public static void writeClassName(final EasyAppender appender, final String className)
   {
      LOG.debug(className);
      //can't use recursion to write the string because that's endless and needs different format
      final byte[] writeMe = className.getBytes(StandardCharsets.UTF_8);
      appender.append(writeMe);
      ByteSerializableStrategy.writeByte(appender, ';');
      //instead of size then string have the string terminated by ; since this saves 3 bytes and class names can't contain ;
   }

   public static String readClassName(final EasyReader reader)
   {
      return readClassName(reader, reader.readByte());
   }

   public static String readClassName(final EasyReader reader, final byte firstByte)
   {
      final ByteArrayOutputStream classNameStream = new ByteArrayOutputStream();
      classNameStream.write(firstByte);
      while (true)
      {
         if (!reader.hasData())
            throw new StreamCorruptedException("Incomplete header: class name not terminated");
         final byte thisByte = reader.readByte();
         if (thisByte == ';') break;
         classNameStream.write(thisByte);
      }
      final String result = classNameStream.toString(StandardCharsets.UTF_8);
      LOG.debug(result);
      return result;
   }
}
