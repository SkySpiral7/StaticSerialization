package com.github.skySpiral7.java.staticSerialization.strategy.generic;

public interface SerializableStrategy
{
   public boolean supports(final byte firstByte, final Class<?> actualClass);
   public void write(final Object data);
   public <T> T read(final Class<T> actualClass);
}
