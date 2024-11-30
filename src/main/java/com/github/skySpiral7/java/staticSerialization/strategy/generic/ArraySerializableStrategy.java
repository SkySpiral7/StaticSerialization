package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamReader;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamWriter;
import com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.IntegerSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ReaderValidationStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.ArrayUtil;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;

import java.lang.reflect.Array;
import java.util.HashMap;
import java.util.Map;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class ArraySerializableStrategy implements HeaderStrategy, DataStrategy
{
   private final ReaderValidationStrategy readerValidationStrategy;
   private final EasyReader reader;
   private final ObjectStreamReader streamReader;
   private final InternalStreamReader internalStreamReader;
   private final InternalStreamWriter internalStreamWriter;
   private final IntegerSerializableStrategy integerSerializableStrategy;
   private final ArrayUtil arrayUtil;
   private final ClassUtil classUtil;
   private final ByteSerializableStrategy byteSerializableStrategy;
   private final StringSerializableStrategy stringSerializableStrategy;
   private final Map<Class<?>, Character> CLASS_TO_COMPRESSED_HEADER;

   {
      CLASS_TO_COMPRESSED_HEADER = new HashMap<>();
      //Boolean has 2 values so it isn't in the map
      CLASS_TO_COMPRESSED_HEADER.put(Byte.class, '~');
      CLASS_TO_COMPRESSED_HEADER.put(Short.class, '!');
      CLASS_TO_COMPRESSED_HEADER.put(Integer.class, '@');
      CLASS_TO_COMPRESSED_HEADER.put(Long.class, '#');
      //$ is allowed to be in class names
      CLASS_TO_COMPRESSED_HEADER.put(Float.class, '%');
      CLASS_TO_COMPRESSED_HEADER.put(Double.class, '^');
      CLASS_TO_COMPRESSED_HEADER.put(Character.class, '\'');
      CLASS_TO_COMPRESSED_HEADER.put(String.class, '"');
   }

   public ArraySerializableStrategy(final ReaderValidationStrategy readerValidationStrategy,
                                    final EasyReader reader,
                                    final ObjectStreamReader streamReader,
                                    final InternalStreamReader internalStreamReader,
                                    final IntegerSerializableStrategy integerSerializableStrategy)
   {
      this.arrayUtil = null;
      this.classUtil = null;
      this.byteSerializableStrategy = null;
      this.stringSerializableStrategy = null;
      this.readerValidationStrategy = readerValidationStrategy;
      this.reader = reader;
      this.streamReader = streamReader;
      this.internalStreamReader = internalStreamReader;
      this.internalStreamWriter = null;
      this.integerSerializableStrategy = integerSerializableStrategy;
   }

   public ArraySerializableStrategy(final UtilInstances utilInstances,
                                    final ByteSerializableStrategy byteSerializableStrategy,
                                    final StringSerializableStrategy stringSerializableStrategy,
                                    final InternalStreamWriter internalStreamWriter,
                                    final IntegerSerializableStrategy integerSerializableStrategy)
   {
      this.arrayUtil = utilInstances.getArrayUtil();
      this.classUtil = utilInstances.getClassUtil();
      this.byteSerializableStrategy = byteSerializableStrategy;
      this.stringSerializableStrategy = stringSerializableStrategy;
      this.readerValidationStrategy = null;
      this.reader = null;
      this.streamReader = null;
      this.internalStreamReader = null;
      this.internalStreamWriter = internalStreamWriter;
      this.integerSerializableStrategy = integerSerializableStrategy;
   }

   @Override
   public boolean supportsReadingHeader(final byte firstByte)
   {
      return ('[' == firstByte || ']' == firstByte);
   }

   @Override
   public HeaderInformation<?> readHeader(final Class<?> inheritFromClass,
                                          final HeaderInformation.PartialHeader partialHeader,
                                          final Class<?> expectedClass,
                                          final boolean allowChildClass)
   {
      final boolean primitiveArray = (']' == partialHeader.firstByte());
      final int dimensionCount = Byte.toUnsignedInt(
         StreamCorruptedException.throwIfNotEnoughData(reader, 1, "Incomplete header: no array dimensions")[0]
      );
      //TODO: shouldn't this return here for primitive 1D arrays? (thin tests)
      final byte componentFirstByte = StreamCorruptedException.throwIfNotEnoughData(reader, 1, "Incomplete header: no array component type")[0];
      if (StringSerializableStrategy.TERMINATOR == componentFirstByte)
         throw new StreamCorruptedException("header's array component type can't be null");
      if ('-' == componentFirstByte) throw new StreamCorruptedException("header's array component type can't be false");

      if ('+' == componentFirstByte)
      {
         final HeaderInformation<?> headerInformation = HeaderInformation.forPossibleArray(componentFirstByte,
            Boolean.class, dimensionCount, primitiveArray);
         readerValidationStrategy.getClassFromHeader(headerInformation, expectedClass, allowChildClass);
         return headerInformation;
      }
      else
      {
         final HeaderInformation.PartialHeader componentPartialHeader = new HeaderInformation.PartialHeader(null,
            componentFirstByte, dimensionCount, primitiveArray);
         final HeaderInformation<?> componentHeaderInfo = internalStreamReader.getAllSerializableStrategy().readHeader(
            inheritFromClass, componentPartialHeader, expectedClass, allowChildClass);
         internalStreamReader.readHeaderClass(componentHeaderInfo, expectedClass, allowChildClass);
         return componentHeaderInfo;
      }
   }

   @Override
   public boolean supportsWritingHeader(final Class<?> inheritFromClass, final Object data)
   {
      return data.getClass().isArray();
   }

   @Override
   public boolean writeHeader(final Class<?> inheritFromClass, final Object data)
   {
      Class<?> baseComponent = arrayUtil.getBaseComponentType(data.getClass());
      if (Object.class.equals(inheritFromClass) || null == inheritFromClass)
      {
         //array indicator and dimension count can be derived from containing array so don't populate it
         //unless I'm inside Object[] in which case I could have new arrays.
         //baseComponent can't be void or null
         if (baseComponent.isPrimitive())
         {
            byteSerializableStrategy.writeByte(']');
            baseComponent = classUtil.boxClass(baseComponent);
         }
         else byteSerializableStrategy.writeByte('[');

         final int dimensionCount = arrayUtil.countArrayDimensions(data.getClass());
         byteSerializableStrategy.writeByte(dimensionCount);  //won't be 0, max: 255. Use unsigned byte
      }

      //TODO: should be recursive
      if (baseComponent.equals(Boolean.class)) byteSerializableStrategy.writeByte('+');
      else if (CLASS_TO_COMPRESSED_HEADER.containsKey(baseComponent))
         byteSerializableStrategy.writeByte(CLASS_TO_COMPRESSED_HEADER.get(baseComponent));
      else
      {
         stringSerializableStrategy.writeData(baseComponent.getName());
      }
      return false;
   }

   @Override
   public boolean supportsData(final Class<?> actualClass)
   {
      return actualClass.isArray();
   }

   @Override
   public void writeData(final Object data)
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
   public <T> T readData(final Class<T> actualClass)
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
