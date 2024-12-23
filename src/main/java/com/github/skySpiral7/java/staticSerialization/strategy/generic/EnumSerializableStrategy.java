package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.strategy.IntegerSerializableStrategy;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class EnumSerializableStrategy implements DataStrategy
{
   private final IntegerSerializableStrategy integerSerializableStrategy;

   public EnumSerializableStrategy(IntegerSerializableStrategy integerSerializableStrategy)
   {
      this.integerSerializableStrategy = integerSerializableStrategy;
   }

   @Override
   public boolean supportsData(final Class<?> actualClass)
   {
      return actualClass.isEnum();
   }

   @Override
   public void writeData(final Object rawData)
   {
      final Enum<?> data = (Enum<?>) rawData;
      integerSerializableStrategy.write(data.ordinal());
   }

   @Override
   public <T> T readData(final Class<T> expectedClass)
   {
      final int ordinal = integerSerializableStrategy.read("Missing enum ordinal");
      if (ordinal < 0) throw new StreamCorruptedException("Invalid enum ordinal. Actual: " + ordinal);

      final Enum<?>[] values = (Enum<?>[]) expectedClass.getEnumConstants();  //won't return null because it is an enum
      if (values.length <= ordinal) throw new StreamCorruptedException(
         String.format("%s.values()[%d] doesn't exist. Actual length: %d", expectedClass.getName(), ordinal, values.length));

      return cast(values[ordinal]);
   }
}
