package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.strategy.IntegerSerializableStrategy;

import java.util.BitSet;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class BitSetSerializableStrategy implements SerializableStrategy
{
   private final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy;
   private final IntegerSerializableStrategy integerSerializableStrategy;

   public BitSetSerializableStrategy(final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy,
                                     final IntegerSerializableStrategy integerSerializableStrategy)
   {
      this.boxPrimitiveSerializableStrategy = boxPrimitiveSerializableStrategy;
      this.integerSerializableStrategy = integerSerializableStrategy;
   }

   @Override
   public boolean supportsData(final Class<?> actualClass)
   {
      return BitSet.class.isAssignableFrom(actualClass);
   }

   @Override
   public void write(final Object rawData)
   {
      final BitSet data = (BitSet) rawData;
      final byte[] compressed = compress(data);
      integerSerializableStrategy.write(compressed.length);
      for (final byte element : compressed)
      {
         boxPrimitiveSerializableStrategy.write(element);
      }
   }

   @Override
   public <T> T read(final Class<T> actualClass)
   {
      final int byteLength = integerSerializableStrategy.read("Missing array length");
      final byte[] byteArray = new byte[byteLength];
      for (int readIndex = 0; readIndex < byteLength; ++readIndex)
      {
         byteArray[readIndex] = boxPrimitiveSerializableStrategy.read(Byte.class);
      }
      return cast(decompress(byteArray));
   }

   public static byte[] compress(final BitSet bitSet)
   {
      return bitSet.toByteArray();
   }

   public static BitSet decompress(final byte[] bytes)
   {
      return BitSet.valueOf(bytes);
   }
}
