package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import java.util.UUID;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class UuidSerializableStrategy implements SerializableStrategy
{
   private final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy;

   public UuidSerializableStrategy(final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy)
   {
      this.boxPrimitiveSerializableStrategy = boxPrimitiveSerializableStrategy;
   }

   @Override
   public boolean supportsData(final Class<?> actualClass)
   {
      return UUID.class.isAssignableFrom(actualClass);
   }

   @Override
   public void write(final Object rawData)
   {
      final UUID data = (UUID) rawData;
      final long[] compressed = compress(data);
      boxPrimitiveSerializableStrategy.write(compressed[0]);
      boxPrimitiveSerializableStrategy.write(compressed[1]);
   }

   @Override
   public <T> T read(final Class<T> expectedClass)
   {
      //TODO: could have more specific "no data" error
      final long[] compressed = {
         boxPrimitiveSerializableStrategy.read(Long.class),
         boxPrimitiveSerializableStrategy.read(Long.class)
      };
      return cast(decompress(compressed));
   }

   /**
    * @return big endian. always length 2
    */
   public static long[] compress(final UUID uuid)
   {
      return new long[]{uuid.getMostSignificantBits(), uuid.getLeastSignificantBits()};
   }

   /**
    * @param bytes big endian. always length 2
    */
   public static UUID decompress(final long[] bytes)
   {
      return new UUID(bytes[0], bytes[1]);
   }
}
