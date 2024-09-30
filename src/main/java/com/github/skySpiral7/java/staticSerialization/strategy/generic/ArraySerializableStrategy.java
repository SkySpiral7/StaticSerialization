package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamReader;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamWriter;
import com.github.skySpiral7.java.staticSerialization.strategy.IntegerSerializableStrategy;

import java.lang.reflect.Array;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class ArraySerializableStrategy implements SerializableStrategy
{
   private final ObjectStreamReader streamReader;
   private final InternalStreamReader internalStreamReader;
   private final InternalStreamWriter internalStreamWriter;
   private final IntegerSerializableStrategy integerSerializableStrategy;

   public ArraySerializableStrategy(final ObjectStreamReader streamReader,
                                    final InternalStreamReader internalStreamReader,
                                    final IntegerSerializableStrategy integerSerializableStrategy)
   {
      this.streamReader = streamReader;
      this.internalStreamReader = internalStreamReader;
      this.internalStreamWriter = null;
      this.integerSerializableStrategy = integerSerializableStrategy;
   }

   public ArraySerializableStrategy(final InternalStreamWriter internalStreamWriter,
                                    final IntegerSerializableStrategy integerSerializableStrategy)
   {
      this.streamReader = null;
      this.internalStreamReader = null;
      this.internalStreamWriter = internalStreamWriter;
      this.integerSerializableStrategy = integerSerializableStrategy;
   }

   @Override
   public boolean supports(final byte firstByte, final Class<?> actualClass)
   {
      return actualClass.isArray();
   }

   @Override
   public void write(final Object data)
   {
      final int length = Array.getLength(data);
      integerSerializableStrategy.write(length);
      final Class<?> componentType = data.getClass().getComponentType();
      for (int writeIndex = 0; writeIndex < length; ++writeIndex)
      {
         final Object element = Array.get(data, writeIndex);
         internalStreamWriter.writeObjectInternal(componentType, element);
      }
   }

   @Override
   public <T> T read(final Class<T> actualClass)
   {
      final Class<?> componentType = actualClass.getComponentType();
      final int arrayLength = integerSerializableStrategy.read("Missing array length");
      final T arrayValue = cast(Array.newInstance(componentType, arrayLength));

      //this is only safe because creating an empty array only requires reading a primitive from stream
      //any constructor that reads objects would register those and mess up the registry order
      //the only reason this needs to be registered at all is because the elements are objects
      streamReader.registerObject(arrayValue);

      for (int readIndex = 0; readIndex < arrayLength; ++readIndex)
      {
         final Object element = internalStreamReader.readObjectInternal(componentType, componentType, true);
         //boolean is the only primitive that could return null
         //TODO: I don't remember why. make sure there's an IT for this
         if (null == element && componentType.isPrimitive())
            throw new StreamCorruptedException("Primitive boolean array can't contain null");
         Array.set(arrayValue, readIndex, element);
      }
      return arrayValue;
   }
}
