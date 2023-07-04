package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.stream.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.stream.AsynchronousFileReader;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public enum EnumSerializableStrategy
{
   ;  //no instances

   public static void write(final AsynchronousFileAppender appender, final Enum<?> data)
   {
      IntegerSerializableStrategy.write(appender, data.ordinal());
   }

   public static <T> T read(final AsynchronousFileReader reader, final Class<T> expectedClass)
   {
      final int ordinal = IntegerSerializableStrategy.read(reader);
      if (ordinal < 0) throw new StreamCorruptedException("expected array index. Actual: " + ordinal);

      final Enum<?>[] values = Enum[].class.cast(expectedClass.getEnumConstants());  //won't return null because it is an enum
      if (values.length <= ordinal) throw new StreamCorruptedException(
            String.format("%s.values()[%d] doesn't exist. Actual length: %d", expectedClass.getName(), ordinal, values.length));

      return cast(values[ordinal]);
   }
}
