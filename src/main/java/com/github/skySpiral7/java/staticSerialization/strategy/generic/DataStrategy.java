package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;

public interface DataStrategy
{
   public boolean supportsData(final Class<?> actualClass, final HeaderInformation.CompressionScenario compressionScenario);

   public void writeData(final Object data);

   public <T> T readData(final Class<T> actualClass);
}
