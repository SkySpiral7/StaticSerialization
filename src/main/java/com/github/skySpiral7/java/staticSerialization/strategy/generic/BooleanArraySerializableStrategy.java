package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.strategy.IntegerSerializableStrategy;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class BooleanArraySerializableStrategy implements DataStrategy
{
   private final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy;
   private final IntegerSerializableStrategy integerSerializableStrategy;

   public BooleanArraySerializableStrategy(final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy,
                                           final IntegerSerializableStrategy integerSerializableStrategy)
   {
      this.boxPrimitiveSerializableStrategy = boxPrimitiveSerializableStrategy;
      this.integerSerializableStrategy = integerSerializableStrategy;
   }

   @Override
   public boolean supportsData(final Class<?> actualClass, final HeaderInformation.CompressionScenario compressionScenario)
   {
      return boolean[].class.equals(actualClass);
   }

   @Override
   public void writeData(final Object rawData)
   {
      final boolean[] flagArray = (boolean[]) rawData;
      integerSerializableStrategy.write(flagArray.length);
      final byte[] compressed = compress(flagArray);
      for (final byte element : compressed)
      {
         boxPrimitiveSerializableStrategy.writeData(element);
      }
   }

   @Override
   public <T> T readData(final Class<T> expectedClass)
   {
      final int flagLength = integerSerializableStrategy.read("Missing array length");
      final int byteLength = (flagLength + 7) / 8;  //rounds up
      final byte[] byteArray = new byte[byteLength];
      for (int readIndex = 0; readIndex < byteLength; ++readIndex)
      {
         byteArray[readIndex] = boxPrimitiveSerializableStrategy.readData(Byte.class);
      }
      return cast(decompress(byteArray, flagLength));
   }

   /**
    * @return little endian (bytes and bits)
    */
   public static byte[] compress(final boolean[] flagArray)
   {
      final byte[] result = new byte[(flagArray.length + 7) / 8];  //rounds up
      for (int booleanIndex = 0; booleanIndex < flagArray.length; booleanIndex++)
      {
         final int byteIndex = booleanIndex / 8;  //rounds down
         final int bitIndex = booleanIndex % 8;
         if (flagArray[booleanIndex])
         {
            final int mask = 1 << bitIndex;
            result[byteIndex] |= (byte) mask;
         }
      }
      return result;
   }

   /**
    * @param bytes  little endian (bytes and bits)
    * @param length the length of the result. not all bits are always used
    */
   public static boolean[] decompress(final byte[] bytes, final int length)
   {
      final boolean[] result = new boolean[length];
      for (int booleanIndex = 0; booleanIndex < length; booleanIndex++)
      {
         final int byteIndex = booleanIndex / 8;  //rounds down
         final int bitIndex = booleanIndex % 8;
         final int mask = 1 << bitIndex;
         result[booleanIndex] = (bytes[byteIndex] & mask) != 0;
      }
      return result;
   }
}
