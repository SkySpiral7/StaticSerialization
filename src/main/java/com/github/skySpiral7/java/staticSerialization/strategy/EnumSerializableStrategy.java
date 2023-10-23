package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class EnumSerializableStrategy
{
   private final IntegerSerializableStrategy integerSerializableStrategy;

   public EnumSerializableStrategy(IntegerSerializableStrategy integerSerializableStrategy)
   {
      this.integerSerializableStrategy = integerSerializableStrategy;
   }

   public void write(final Enum<?> data)
   {
      integerSerializableStrategy.write(data.ordinal());
   }

   public <T> T read(final Class<T> expectedClass)
   {
      final int ordinal = integerSerializableStrategy.read("Missing enum ordinal");
      if (ordinal < 0) throw new StreamCorruptedException("Invalid enum ordinal. Actual: " + ordinal);

      final Enum<?>[] values = Enum[].class.cast(expectedClass.getEnumConstants());  //won't return null because it is an enum
      if (values.length <= ordinal) throw new StreamCorruptedException(
         String.format("%s.values()[%d] doesn't exist. Actual length: %d", expectedClass.getName(), ordinal, values.length));

      return cast(values[ordinal]);
   }
}
