package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamReader;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamWriter;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public enum EnumSerializableStrategy
{
   ;  //no instances

   public static void write(final InternalStreamWriter internalStreamWriter, final Enum<?> data)
   {
      IntegerSerializableStrategy.write(internalStreamWriter, data.ordinal());
   }

   public static <T> T read(final InternalStreamReader internalStreamReader, final Class<T> expectedClass)
   {
      final int ordinal = IntegerSerializableStrategy.read(internalStreamReader, "Missing enum ordinal");
      if (ordinal < 0) throw new StreamCorruptedException("Invalid enum ordinal. Actual: " + ordinal);

      final Enum<?>[] values = Enum[].class.cast(expectedClass.getEnumConstants());  //won't return null because it is an enum
      if (values.length <= ordinal) throw new StreamCorruptedException(
         String.format("%s.values()[%d] doesn't exist. Actual length: %d", expectedClass.getName(), ordinal, values.length));

      return cast(values[ordinal]);
   }
}
