package com.github.skySpiral7.java.staticSerialization.strategy;

import java.lang.reflect.Array;

import com.github.skySpiral7.java.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;

import static com.github.skySpiral7.java.util.ClassUtil.cast;

public enum ArraySerializableStrategy
{
   ;  //no instances

   public static void write(final ObjectStreamWriter writer, final Object data)
   {
      final int length = Array.getLength(data);
      for (int writeIndex = 0; writeIndex < length; ++writeIndex)
      {
         writer.writeObject(Array.get(data, writeIndex));
      }
   }

   public static <T> T read(final ObjectStreamReader objectStreamReader, final AsynchronousFileReader fileReader,
                            final Class<?> componentType)
   {
      final int arrayLength = IntegerSerializableStrategy.read(fileReader);
      final Object arrayValue = Array.newInstance(componentType, arrayLength);
      for (int readIndex = 0; readIndex < arrayLength; ++readIndex)
      {
         Array.set(arrayValue, readIndex, objectStreamReader.readObject(componentType));
      }
      return cast(arrayValue);
   }
}
