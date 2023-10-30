package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamReader;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamWriter;

import java.lang.reflect.Array;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class ArraySerializableStrategy
{
   private final IntegerSerializableStrategy integerSerializableStrategy;

   public ArraySerializableStrategy(IntegerSerializableStrategy integerSerializableStrategy)
   {
      this.integerSerializableStrategy = integerSerializableStrategy;
   }

   public void write(final ObjectStreamWriter streamWriter, final InternalStreamWriter internalStreamWriter,
                     final Object data)
   {
      final int length = Array.getLength(data);
      integerSerializableStrategy.write(length);
      final Class<?> componentType = data.getClass().getComponentType();
      for (int writeIndex = 0; writeIndex < length; ++writeIndex)
      {
         final Object element = Array.get(data, writeIndex);
         internalStreamWriter.writeObjectInternal(streamWriter, componentType, element);
      }
   }

   public <T_Array, T_Component> T_Array read(final ObjectStreamReader streamReader, final InternalStreamReader internalStreamReader,
                                              final Class<T_Component> componentType)
   {
      final int arrayLength = integerSerializableStrategy.read("Missing " +
         "array length");
      final T_Array arrayValue = cast(Array.newInstance(componentType, arrayLength));

      //this is only safe because creating an empty array only requires reading a primitive from stream
      //any constructor that reads objects would register those and mess up the registry order
      //the only reason this needs to be registered at all is because the elements are objects
      streamReader.registerObject(arrayValue);

      for (int readIndex = 0; readIndex < arrayLength; ++readIndex)
      {
         final T_Component element = internalStreamReader.readObjectInternal(componentType, componentType, true);
         //boolean is the only primitive that could return null
         //TODO: I don't remember why. make sure there's an IT for this
         if (null == element && componentType.isPrimitive())
            throw new StreamCorruptedException("Primitive boolean array can't contain null");
         Array.set(arrayValue, readIndex, element);
      }
      return arrayValue;
   }
}
