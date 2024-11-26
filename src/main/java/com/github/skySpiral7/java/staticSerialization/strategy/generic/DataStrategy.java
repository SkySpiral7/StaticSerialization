package com.github.skySpiral7.java.staticSerialization.strategy.generic;

public interface DataStrategy
{
   public boolean supportsData(final Class<?> actualClass);

   public void writeData(final Object data);

   public <T> T readData(final Class<T> actualClass);
}
