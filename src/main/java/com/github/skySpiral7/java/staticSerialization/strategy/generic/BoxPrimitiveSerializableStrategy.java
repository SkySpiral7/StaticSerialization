package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.IntegerSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ReaderValidationStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ShortSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.BitWiseUtil;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;

import java.util.HashMap;
import java.util.Map;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class BoxPrimitiveSerializableStrategy implements SerializableStrategy
{
   private final Map<Character, Class<?>> COMPRESSED_HEADER_TO_CLASS;

   {
      COMPRESSED_HEADER_TO_CLASS = new HashMap<>();
      COMPRESSED_HEADER_TO_CLASS.put('~', Byte.class);
      COMPRESSED_HEADER_TO_CLASS.put('!', Short.class);
      COMPRESSED_HEADER_TO_CLASS.put('@', Integer.class);
      COMPRESSED_HEADER_TO_CLASS.put('#', Long.class);
      COMPRESSED_HEADER_TO_CLASS.put('%', Float.class);
      COMPRESSED_HEADER_TO_CLASS.put('^', Double.class);
      COMPRESSED_HEADER_TO_CLASS.put('\'', Character.class);
   }

   private final EasyReader reader;
   private final BitWiseUtil bitWiseUtil;
   private final ClassUtil classUtil;
   private final ReaderValidationStrategy readerValidationStrategy;
   private final ByteSerializableStrategy byteSerializableStrategy;
   private final ShortSerializableStrategy shortSerializableStrategy;
   private final IntegerSerializableStrategy integerSerializableStrategy;

   /**
    * For writing
    */
   public BoxPrimitiveSerializableStrategy(final UtilInstances utilInstances,
                                           final ByteSerializableStrategy byteSerializableStrategy,
                                           final IntegerSerializableStrategy integerSerializableStrategy)
   {
      this.reader = null;
      this.bitWiseUtil = utilInstances.getBitWiseUtil();
      this.classUtil = utilInstances.getClassUtil();
      this.readerValidationStrategy = null;
      this.byteSerializableStrategy = byteSerializableStrategy;
      this.shortSerializableStrategy = null;
      this.integerSerializableStrategy = integerSerializableStrategy;
   }

   public BoxPrimitiveSerializableStrategy(final EasyReader reader, final UtilInstances utilInstances,
                                           final ReaderValidationStrategy readerValidationStrategy,
                                           final ShortSerializableStrategy shortSerializableStrategy,
                                           final IntegerSerializableStrategy integerSerializableStrategy)
   {
      this.reader = reader;
      this.bitWiseUtil = utilInstances.getBitWiseUtil();
      this.classUtil = utilInstances.getClassUtil();
      this.readerValidationStrategy = readerValidationStrategy;
      this.byteSerializableStrategy = null;
      this.shortSerializableStrategy = shortSerializableStrategy;
      this.integerSerializableStrategy = integerSerializableStrategy;
   }

   @Override
   public boolean supportsHeader(final byte firstByte)
   {
      return COMPRESSED_HEADER_TO_CLASS.containsKey((char) firstByte);  //safe cast because map contains only ASCII
   }

   @Override
   public Class<?> readHeader(final Class<?> inheritFromClass, final HeaderSerializableStrategy.PartialHeader partialHeader, final Class<?> expectedClass, final boolean allowChildClass)
   {
      final Class<?> headerClass = COMPRESSED_HEADER_TO_CLASS.get((char) partialHeader.firstByte());  //safe cast because map contains only ASCII
      final HeaderInformation<?> headerInformation = HeaderInformation.forPossibleArray(partialHeader.firstByte(),
         headerClass, partialHeader.dimensionCount(), partialHeader.primitiveArray());
      return readerValidationStrategy.getClassFromHeader(headerInformation, expectedClass, allowChildClass);
   }

   @Override
   public boolean supportsData(final Class<?> actualClass)
   {
      return classUtil.isPrimitiveOrBox(actualClass);
   }

   @Override
   public void write(final Object data)
   {
      if (data instanceof Byte) byteSerializableStrategy.writeByte((byte) data);
      else if (data instanceof Short) byteSerializableStrategy.writeBytes((short) data, 2);
      else if (data instanceof Integer) integerSerializableStrategy.write((int) data);
      else if (data instanceof Long) byteSerializableStrategy.writeBytes((long) data, 8);
      else if (data instanceof Float)
      {
         final int castedData = Float.floatToIntBits((float) data);
         //intentionally normalizes NaN
         //TODO: does it need to normalize NaN?
         byteSerializableStrategy.writeBytes(castedData, 4);
      }
      else if (data instanceof Double)
      {
         final long castedData = Double.doubleToLongBits((double) data);
         //intentionally normalizes NaN
         byteSerializableStrategy.writeBytes(castedData, 8);
      }
      //Boolean won't come here because the value is header only
      //TODO: can a null Boolean[] get here?
      else if (data instanceof Character) byteSerializableStrategy.writeBytes((char) data, 2);
      else throw new AssertionError("Method shouldn't've been called");
   }

   @Override
   public <T> T read(final Class<T> expectedClass)
   {
      if (Byte.class.equals(expectedClass))
      {
         return cast(StreamCorruptedException.throwIfNotEnoughData(reader, 1, "Missing byte data")[0]);
      }
      if (Short.class.equals(expectedClass))
      {
         return cast(shortSerializableStrategy.read("Missing short data"));
      }
      if (Integer.class.equals(expectedClass))
      {
         return cast(integerSerializableStrategy.read("Missing int data"));
      }
      if (Long.class.equals(expectedClass))
      {
         return cast(bitWiseUtil.bigEndianBytesToLong(
            StreamCorruptedException.throwIfNotEnoughData(reader, 8, "Missing long data")
         ));
      }
      if (Float.class.equals(expectedClass))
      {
         final byte[] data = StreamCorruptedException.throwIfNotEnoughData(reader, 4, "Missing float data");
         final int intData = bitWiseUtil.bigEndianBytesToInteger(data);
         return cast(Float.intBitsToFloat(intData));
      }
      if (Double.class.equals(expectedClass))
      {
         final byte[] data = StreamCorruptedException.throwIfNotEnoughData(reader, 8, "Missing double data");
         final long longData = bitWiseUtil.bigEndianBytesToLong(data);
         return cast(Double.longBitsToDouble(longData));
      }
      if (Boolean.class.equals(expectedClass))
      {
         //TODO: should be reachable through Boolean[] as well. else forbid null here
         //Code is only reachable through primitive boolean arrays, else the header contains the value.
         //Also reachable if a custom written stream uses a header of Boolean.class.
         final byte data = StreamCorruptedException.throwIfNotEnoughData(reader, 1, "Missing boolean data")[0];
         if ('+' == data) return cast(Boolean.TRUE);
         if ('-' == data) return cast(Boolean.FALSE);
         if (StringSerializableStrategy.TERMINATOR == data) return null;
         throw new StreamCorruptedException(data + " is not a boolean value");
      }
      if (Character.class.equals(expectedClass))
      {
         return cast((char) shortSerializableStrategy.read("Missing char data"));
      }

      throw new AssertionError("Method shouldn't've been called");
   }
}
