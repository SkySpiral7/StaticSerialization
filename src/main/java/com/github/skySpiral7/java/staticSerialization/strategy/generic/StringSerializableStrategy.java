package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy;
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
   /**
    * Used as a string terminating byte. The value isn't valid UTF-8.
    */
   public static final byte TERMINATOR = (byte) 0xFF;
   private final EasyReader reader;
   private final EasyAppender appender;
   private final ByteSerializableStrategy byteSerializableStrategy;

   public StringSerializableStrategy(final EasyReader reader)
   {
      this.reader = reader;
      appender = null;
      this.byteSerializableStrategy = null;
   }

   public StringSerializableStrategy(final EasyAppender appender,
                                     final ByteSerializableStrategy byteSerializableStrategy)
   {
      reader = null;
      this.appender = appender;
      this.byteSerializableStrategy = byteSerializableStrategy;
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
      appender.append(data.getBytes(StandardCharsets.UTF_8));
      byteSerializableStrategy.writeByte(TERMINATOR);
   }

   @Override
   public <T> T read(final Class<T> actualClass)
   {
      final ByteArrayOutputStream classNameStream = new ByteArrayOutputStream();
      byte[] remaining = StreamCorruptedException.throwIfNotByteTerminated(reader, TERMINATOR, "String data not " +
         "terminated");
      //-1 to exclude the terminator
      classNameStream.write(remaining, 0, remaining.length - 1);
      final String result = classNameStream.toString(StandardCharsets.UTF_8);
      LOG.debug(result);
      return cast(result);
   }

   public void writeClassName(final String className)
   {
      LOG.debug(className);
      final byte[] writeMe = className.getBytes(StandardCharsets.UTF_8);
      appender.append(writeMe);
      byteSerializableStrategy.writeByte(TERMINATOR);
   }

   public String readClassName(final byte firstByte)
   {
      final ByteArrayOutputStream classNameStream = new ByteArrayOutputStream();
      classNameStream.write(firstByte);
      byte[] remaining = StreamCorruptedException.throwIfNotByteTerminated(reader, TERMINATOR, "Incomplete header: " +
         "class name not terminated");
      //-1 to exclude the terminator
      classNameStream.write(remaining, 0, remaining.length - 1);
      final String result = classNameStream.toString(StandardCharsets.UTF_8);
      LOG.debug(result);
      return result;
   }
}
