package com.github.skySpiral7.java.staticSerialization.strategy.generic;

public interface SerializableStrategy
{
   public default boolean supportsHeader(final byte firstByte)
   {
      return false;
   }

   public default Class<?> readHeader(final Class<?> inheritFromClass, final byte firstByte)
   {
      return null;
   }

   public boolean supportsData(final Class<?> actualClass);
   public void write(final Object data);
   public <T> T read(final Class<T> actualClass);
}
