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
   /**
    * Used as a string terminating byte. The value isn't valid UTF-8.
    */
   public static final byte TERMINATOR = (byte) 0xFF;
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
      //TODO: could use FF delimiter (invalid UTF-8) to reduce overhead by 3
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
      byteSerializableStrategy.writeByte(TERMINATOR);
      //instead of size then string have the string terminated by ; since this saves 3 bytes and class names can't contain ;
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
