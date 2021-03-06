package com.github.skySpiral7.java.staticSerialization.strategy;

import java.lang.reflect.Array;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.fileWrapper.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.fileWrapper.AsynchronousFileReader;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public enum ArraySerializableStrategy
{
   ;  //no instances

   public static void write(final ObjectStreamWriter streamWriter, final InternalStreamWriter internalStreamWriter,
                            final AsynchronousFileAppender fileAppender, final Object data)
   {
      //TODO: Object[] should always include a UUID because it can reference itself
      final int length = Array.getLength(data);
      IntegerSerializableStrategy.write(fileAppender, length);
      final Class<?> componentType = data.getClass().getComponentType();
      for (int writeIndex = 0; writeIndex < length; ++writeIndex)
      {
         final Object element = Array.get(data, writeIndex);
         internalStreamWriter.writeObjectInternal(streamWriter, componentType, element);
      }
   }

   public static <T_Array, T_Component> T_Array read(final ObjectStreamReader streamReader, final InternalStreamReader internalStreamReader,
                                                     final AsynchronousFileReader fileReader, final Class<T_Component> componentType)
   {
      final int arrayLength = IntegerSerializableStrategy.read(fileReader);
      final T_Array arrayValue = cast(Array.newInstance(componentType, arrayLength));
      for (int readIndex = 0; readIndex < arrayLength; ++readIndex)
      {
         final T_Component element = internalStreamReader.readObjectInternal(streamReader, componentType, componentType, true);
         //boolean is the only primitive that could return null
         //TODO: I don't remember why. make sure there's an IT for this
         if (null == element && componentType.isPrimitive())
            throw new StreamCorruptedException("Primitive boolean array can't contain null");
         Array.set(arrayValue, readIndex, element);
      }
      return arrayValue;
   }
}
