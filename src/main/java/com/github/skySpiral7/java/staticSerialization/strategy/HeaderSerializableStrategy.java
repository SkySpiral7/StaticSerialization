package com.github.skySpiral7.java.staticSerialization.strategy;

import java.util.HashMap;
import java.util.Map;

import com.github.skySpiral7.java.AsynchronousFileAppender;
import com.github.skySpiral7.java.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.util.ArrayUtil;
import com.github.skySpiral7.java.util.ClassUtil;

import static com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy.writeByte;

public enum HeaderSerializableStrategy
{
   ;  //no instances

   /**
    * Not in map:
    * <ul>
    * <li>+ boolean true</li>
    * <li>- boolean false</li>
    * <li>[2 object arrays</li>
    * <li>]2 primitive arrays</li>
    * <li>; null</li>
    * </ul>
    */
   private static final Map<Character, Class<?>> COMPRESSED_HEADER_TO_CLASS;
   /**
    * Not in map:
    * <ul>
    * <li>+ boolean true</li>
    * <li>- boolean false</li>
    * <li>[2 object arrays</li>
    * <li>]2 primitive arrays</li>
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
      final boolean primitiveArray = (']' == firstByte);
      if ('[' == firstByte || ']' == firstByte)
      {
         if (reader.remainingBytes() == 0) throw new StreamCorruptedException("Incomplete header: no array dimensions");
         dimensionCount = Byte.toUnsignedInt(reader.readByte());
         if (reader.remainingBytes() == 0) throw new StreamCorruptedException("Incomplete header: no array component type");
         firstByte = reader.readByte();
         if (';' == firstByte || '-' == firstByte)
            throw new StreamCorruptedException("header's array component type can't be null or false");
         if ('+' == firstByte) return new HeaderInformation(Boolean.class.getName(), dimensionCount, primitiveArray);
      }
      else dimensionCount = 0;

      //TODO: add assumed type '?' for arrays (see notes)
      if (';' == firstByte) return new HeaderInformation();  //the empty string class name means null
      if ('+' == firstByte) return new HeaderInformation(Boolean.TRUE);
      if ('-' == firstByte) return new HeaderInformation(Boolean.FALSE);
      if (COMPRESSED_HEADER_TO_CLASS.containsKey((char) firstByte))
      {
         final Class<?> compressedClass = COMPRESSED_HEADER_TO_CLASS.get((char) firstByte);
         return new HeaderInformation(compressedClass.getName(), dimensionCount, primitiveArray);
      }

      //else firstByte is part of a class name
      return new HeaderInformation(StringSerializableStrategy.readClassName(reader, firstByte), dimensionCount, primitiveArray);
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
         Class<?> baseComponent = ArrayUtil.getBaseComponentType(data.getClass());
         //array type can't be void or null
         if (baseComponent.isPrimitive())
         {
            writeByte(appender, ']');
            baseComponent = ClassUtil.boxClass(baseComponent);
         }
         else writeByte(appender, '[');

         final int dimensionCount = ArrayUtil.countArrayDimensions(data.getClass());
         writeByte(appender, dimensionCount);  //won't be 0, max: 255. Use unsigned byte

         if (baseComponent.equals(Boolean.class)) writeByte(appender, '+');
         else if (ClassUtil.isBoxedPrimitive(baseComponent)) writeByte(appender, CLASS_TO_COMPRESSED_HEADER.get(baseComponent));
         else
         {
            StringSerializableStrategy.writeClassName(appender, baseComponent.getName());
         }
      }
      else
      {
         StringSerializableStrategy.writeClassName(appender, data.getClass().getName());
      }
   }
}
