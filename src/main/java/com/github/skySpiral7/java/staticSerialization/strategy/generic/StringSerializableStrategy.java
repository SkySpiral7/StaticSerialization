package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.IntegerSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class StringSerializableStrategy implements SerializableStrategy
{
   private static final Logger LOG = LogManager.getLogger();
   private final EasyReader reader;
   private final EasyAppender appender;
   private final ByteSerializableStrategy byteSerializableStrategy;
   private final IntegerSerializableStrategy integerSerializableStrategy;

   public StringSerializableStrategy(final EasyReader reader,
                                     final IntegerSerializableStrategy integerSerializableStrategy)
   {
      this.reader = reader;
      appender = null;
      this.byteSerializableStrategy = null;
      this.integerSerializableStrategy = integerSerializableStrategy;
   }

   public StringSerializableStrategy(final EasyAppender appender,
                                     final ByteSerializableStrategy byteSerializableStrategy,
                                     final IntegerSerializableStrategy integerSerializableStrategy)
   {
      reader = null;
      this.appender = appender;
      this.byteSerializableStrategy = byteSerializableStrategy;
      this.integerSerializableStrategy = integerSerializableStrategy;
   }

   @Override
   public boolean supports(final Class<?> actualClass)
   {
      return String.class.isAssignableFrom(actualClass);
   }

   @Override
   public void write(final Object rawData)
   {
      final String data = (String) rawData;
      LOG.debug(data);
      final byte[] writeMe = data.getBytes(StandardCharsets.UTF_8);
      integerSerializableStrategy.write(writeMe.length);
      appender.append(writeMe);
   }

   @Override
   public <T> T read(final Class<T> actualClass)
   {
      final int stringByteLength = integerSerializableStrategy.read("Missing string byte length");
      /*TODO: could use an Overlong null delimiter 0xC080 to reduce overhead by 2 but harder to read stream
      could also make a new string type for null delimited 0x00 (only used when contains no null)
      AsynchronousFileReader would need a readBytesUntil*/
      byte[] stringData = StreamCorruptedException.throwIfNotEnoughData(reader, stringByteLength, "Missing string data");
      final String result = new String(stringData, StandardCharsets.UTF_8);
      LOG.debug(result);
      return cast(result);
   }

   public void writeClassName(final String className)
   {
      LOG.debug(className);
      //can't use recursion to write the string because that's endless and needs different format
      final byte[] writeMe = className.getBytes(StandardCharsets.UTF_8);
      appender.append(writeMe);
      byteSerializableStrategy.writeByte(';');
      //instead of size then string have the string terminated by ; since this saves 3 bytes and class names can't contain ;
   }

   public String readClassName(final byte firstByte)
   {
      final ByteArrayOutputStream classNameStream = new ByteArrayOutputStream();
      classNameStream.write(firstByte);
      while (true)
      {
         final byte thisByte = StreamCorruptedException.throwIfNotEnoughData(reader, 1, "Incomplete header: class name not terminated")[0];
         if (thisByte == ';') break;
         classNameStream.write(thisByte);
      }
      final String result = classNameStream.toString(StandardCharsets.UTF_8);
      LOG.debug(result);
      return result;
   }
}
