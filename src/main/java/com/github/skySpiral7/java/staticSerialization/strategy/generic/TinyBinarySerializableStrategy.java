package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.strategy.ReaderValidationStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.BitWiseUtil;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

/**
 * @see HeaderInformation.CompressionScenario#TINY_BINARY
 */
public class TinyBinarySerializableStrategy implements HeaderStrategy, DataStrategy
{
   private final ReaderValidationStrategy readerValidationStrategy;
   private final EasyReader reader;
   private final EasyAppender appender;

   public TinyBinarySerializableStrategy(final ReaderValidationStrategy readerValidationStrategy, final EasyReader reader) {
      this.readerValidationStrategy = readerValidationStrategy;
      this.reader = reader;
      this.appender = null;
   }

   public TinyBinarySerializableStrategy(final EasyAppender appender) {
      this.readerValidationStrategy = null;
      this.reader = null;
      this.appender = appender;
   }

   @Override
   public boolean supportsReadingHeader(final byte firstByte) {
      return firstByte == '=';
   }

   @Override
   public HeaderInformation<?> readHeader(final Class<?> inheritFromClass, final HeaderInformation.PartialHeader partialHeader,
                                          final Class<?> expectedClass, final boolean allowChildClass) {
      final HeaderInformation<?> headerInformation = HeaderInformation.forPossibleArray(
              Byte.class, HeaderInformation.CompressionScenario.TINY_BINARY, 1, true);
      //TODO: if they all must do this then at least add to doc
      readerValidationStrategy.getClassFromHeader(headerInformation, expectedClass, allowChildClass);
      return headerInformation;
   }

   @Override
   public boolean supportsWritingHeader(final Class<?> inheritFromClass, final Object data) {
      if(data instanceof final byte[] castData){
         return castData.length <= 255;
      }
      return false;
   }

   @Override
   public boolean writeHeader(final Class<?> inheritFromClass, final Object data) {
      appender.append((byte) '=');
      return false;
   }

   @Override
   public HeaderInformation.CompressionScenario determineCompressionScenario(final Object data) {
      return HeaderInformation.CompressionScenario.TINY_BINARY;
   }

   @Override
   public boolean supportsData(final Class<?> actualClass, final HeaderInformation.CompressionScenario compressionScenario)
   {
      //byte[].class.equals(actualClass) is always also true
      return compressionScenario == HeaderInformation.CompressionScenario.TINY_BINARY;
   }

   @Override
   public void writeData(final Object rawData)
   {
      final byte[] binary = (byte[]) rawData;
      appender.append((byte) binary.length);
      appender.append(binary);
   }

   @Override
   public <T> T readData(final Class<T> expectedClass, final HeaderInformation.CompressionScenario compressionScenario)
   {
      final byte byteLength = StreamCorruptedException.throwIfNotEnoughData(reader, 1, "Missing array length")[0];
      final int binaryLength = 0xFF & byteLength;
      final byte[] binary = StreamCorruptedException.throwIfNotEnoughData(reader, binaryLength, "Not enough binary");
      return cast(binary);
   }
}
