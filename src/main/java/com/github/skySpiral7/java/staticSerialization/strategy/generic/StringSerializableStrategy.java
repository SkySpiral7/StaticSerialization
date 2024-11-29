package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ReaderValidationStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class StringSerializableStrategy implements HeaderStrategy, DataStrategy
{
   private static final Logger LOG = LogManager.getLogger();
   /**
    * Used as a string terminating byte. The value isn't valid UTF-8.
    */
   public static final byte TERMINATOR = (byte) 0xFF;
   private final ReaderValidationStrategy readerValidationStrategy;
   private final EasyReader reader;
   private final EasyAppender appender;
   private final ByteSerializableStrategy byteSerializableStrategy;

   public StringSerializableStrategy(final ReaderValidationStrategy readerValidationStrategy,
                                     final EasyReader reader)
   {
      this.readerValidationStrategy = readerValidationStrategy;
      this.reader = reader;
      appender = null;
      this.byteSerializableStrategy = null;
   }

   public StringSerializableStrategy(final EasyAppender appender,
                                     final ByteSerializableStrategy byteSerializableStrategy)
   {
      readerValidationStrategy = null;
      reader = null;
      this.appender = appender;
      this.byteSerializableStrategy = byteSerializableStrategy;
   }

   @Override
   public boolean supportsReadingHeader(final byte firstByte)
   {
      return firstByte == '"';
   }

   @Override
   public HeaderInformation<?> readHeader(final Class<?> inheritFromClass,
                                          final HeaderSerializableStrategy.PartialHeader partialHeader,
                                          final Class<?> expectedClass,
                                          final boolean allowChildClass)
   {
      final HeaderInformation<?> headerInformation = HeaderInformation.forPossibleArray((byte) '"',
         String.class, partialHeader.dimensionCount(), false);
      readerValidationStrategy.getClassFromHeader(headerInformation, expectedClass, allowChildClass);
      return headerInformation;
   }

   @Override
   public boolean supportsWritingHeader(final Object data)
   {
      return false;
   }

   @Override
   public boolean writeHeader(final Class<?> inheritFromClass, final Object data)
   {
      throw new IllegalStateException("Not implemented");
   }

   @Override
   public boolean supportsData(final Class<?> actualClass)
   {
      return String.class.isAssignableFrom(actualClass);
   }

   @Override
   public void writeData(final Object rawData)
   {
      final String data = (String) rawData;
      LOG.debug(data);
      appender.append(data.getBytes(StandardCharsets.UTF_8));
      byteSerializableStrategy.writeByte(TERMINATOR);
   }

   @Override
   public <T> T readData(final Class<T> actualClass)
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
}
