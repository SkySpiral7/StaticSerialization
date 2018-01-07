package com.github.skySpiral7.java.staticSerialization.strategy;

import java.lang.reflect.Array;
import java.util.HashMap;
import java.util.Map;

import com.github.skySpiral7.java.AsynchronousFileAppender;
import com.github.skySpiral7.java.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.util.ArrayUtil;

import static com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy.writeByte;

public enum HeaderSerializableStrategy
{
   ;  //no instances

   /**
    * Not in map:
    * <ul>
    * <li>+ boolean true</li>
    * <li>- boolean false</li>
    * <li>[2 arrays</li>
    * <li>; null</li>
    * </ul>
    */
   private static final Map<Character, Class<?>> COMPRESSED_HEADER_TO_CLASS;
   /**
    * Not in map:
    * <ul>
    * <li>+ boolean true</li>
    * <li>- boolean false</li>
    * <li>[2 arrays</li>
    * <li>; null</li>
    * </ul>
    */
   private static final Map<Class<?>, Character> CLASS_TO_COMPRESSED_HEADER;

   static
   {
      COMPRESSED_HEADER_TO_CLASS = new HashMap<>();
      COMPRESSED_HEADER_TO_CLASS.put('~', Byte.class);
      COMPRESSED_HEADER_TO_CLASS.put('!', Short.class);
      COMPRESSED_HEADER_TO_CLASS.put('@', Integer.class);
      COMPRESSED_HEADER_TO_CLASS.put('#', Long.class);
      COMPRESSED_HEADER_TO_CLASS.put('%', Float.class);
      COMPRESSED_HEADER_TO_CLASS.put('^', Double.class);
      COMPRESSED_HEADER_TO_CLASS.put('&', Character.class);
      COMPRESSED_HEADER_TO_CLASS.put('*', String.class);

      CLASS_TO_COMPRESSED_HEADER = new HashMap<>();
      CLASS_TO_COMPRESSED_HEADER.put(Byte.class, '~');
      CLASS_TO_COMPRESSED_HEADER.put(Short.class, '!');
      CLASS_TO_COMPRESSED_HEADER.put(Integer.class, '@');
      CLASS_TO_COMPRESSED_HEADER.put(Long.class, '#');
      CLASS_TO_COMPRESSED_HEADER.put(Float.class, '%');
      CLASS_TO_COMPRESSED_HEADER.put(Double.class, '^');
      CLASS_TO_COMPRESSED_HEADER.put(Character.class, '&');
      CLASS_TO_COMPRESSED_HEADER.put(String.class, '*');
   }

   public static HeaderInformation readHeader(final AsynchronousFileReader reader)
   {
      byte firstByte = reader.readByte();

      final int dimensionCount;
      if ('[' == firstByte)
      {
         if (reader.remainingBytes() == 0) throw new StreamCorruptedException("Incomplete header: no array dimensions");
         dimensionCount = Byte.toUnsignedInt(reader.readByte());
         if (reader.remainingBytes() == 0) throw new StreamCorruptedException("Incomplete header: no array component type");
         firstByte = reader.readByte();
         if (';' == firstByte || '-' == firstByte)
            throw new StreamCorruptedException("header's array component type can't be null or false");
         if ('+' == firstByte) return new HeaderInformation(Boolean.class.getName(), dimensionCount);
      }
      else dimensionCount = 0;

      if (';' == firstByte) return new HeaderInformation();  //the empty string class name means null
      if ('+' == firstByte) return new HeaderInformation(Boolean.TRUE);
      if ('-' == firstByte) return new HeaderInformation(Boolean.FALSE);
      if (COMPRESSED_HEADER_TO_CLASS.containsKey((char) firstByte))
      {
         final Class<?> compressedClass = COMPRESSED_HEADER_TO_CLASS.get((char) firstByte);
         return new HeaderInformation(compressedClass.getName(), dimensionCount);
      }

      //else firstByte is part of a class name
      return new HeaderInformation(StringSerializableStrategy.readClassName(reader, firstByte), dimensionCount);
   }

   public static void writeHeader(final AsynchronousFileAppender appender, final Object data)
   {
      if (Boolean.TRUE.equals(data)) writeByte(appender, '+');
      else if (Boolean.FALSE.equals(data)) writeByte(appender, '-');
      else if (data == null) writeByte(appender, ';');  //if data is null then class name is the empty string
      else if (CLASS_TO_COMPRESSED_HEADER.containsKey(data.getClass()))
         writeByte(appender, CLASS_TO_COMPRESSED_HEADER.get(data.getClass()));
      else if (data.getClass().isArray())
      {
         writeByte(appender, '[');
         final int dimensionCount = ArrayUtil.countArrayDimensions(data.getClass());
         //TODO: test and remove tests
         if (dimensionCount > 1) throw new UnsupportedOperationException("Currently only 1d arrays are supported");
         writeByte(appender, dimensionCount);  //won't be 0, max: 255. Use unsigned byte
         final Class<?> baseComponent = ArrayUtil.getBaseComponentType(data.getClass());
         //array type can't be void or null
         //TODO: support primitive arrays
         //         if (baseComponent.equals(boolean.class)) writeByte(appender, '+');
         //         else if (baseComponent.isPrimitive()) writeByte(COMPRESSED_CLASSES.get(ClassUtil.boxClass(baseComponent)));
         //            //TODO: compress arrays of String and Boxes
         //            //else if (ClassUtil.isBoxedPrimitive(baseComponent))
         //         else
         {
            StringSerializableStrategy.writeClassName(appender, baseComponent.getName());
         }
         IntegerSerializableStrategy.write(appender, Array.getLength(data));
      }
      else
      {
         StringSerializableStrategy.writeClassName(appender, data.getClass().getName());
      }
   }
}
