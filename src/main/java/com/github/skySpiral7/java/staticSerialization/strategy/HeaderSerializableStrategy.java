package com.github.skySpiral7.java.staticSerialization.strategy;

import java.util.HashMap;
import java.util.Map;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.fileWrapper.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.fileWrapper.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.util.ArrayUtil;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;

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
    * <li>? inherit type from containing array</li>
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
    * <li>? inherit type from containing array</li>
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
      //$ is allowed to be in class names
      COMPRESSED_HEADER_TO_CLASS.put('%', Float.class);
      COMPRESSED_HEADER_TO_CLASS.put('^', Double.class);
      COMPRESSED_HEADER_TO_CLASS.put('&', Character.class);
      COMPRESSED_HEADER_TO_CLASS.put('*', String.class);

      CLASS_TO_COMPRESSED_HEADER = new HashMap<>();
      CLASS_TO_COMPRESSED_HEADER.put(Byte.class, '~');
      CLASS_TO_COMPRESSED_HEADER.put(Short.class, '!');
      CLASS_TO_COMPRESSED_HEADER.put(Integer.class, '@');
      CLASS_TO_COMPRESSED_HEADER.put(Long.class, '#');
      //$ is allowed to be in class names
      CLASS_TO_COMPRESSED_HEADER.put(Float.class, '%');
      CLASS_TO_COMPRESSED_HEADER.put(Double.class, '^');
      CLASS_TO_COMPRESSED_HEADER.put(Character.class, '&');
      CLASS_TO_COMPRESSED_HEADER.put(String.class, '*');
   }

   /**
    * @param inheritFromClass the component type of the containing array. null if not currently inside an array.
    */
   public static HeaderInformation readHeader(final AsynchronousFileReader reader, final Class<?> inheritFromClass)
   {
      byte firstByte;
      final int dimensionCount;
      final boolean primitiveArray;

      if (null != inheritFromClass && !Object.class.equals(inheritFromClass))
      {
         //TODO: tests are likely thin
         //inheritFromClass is never primitive void.class.
         //It is only primitive if contained in a primitive array in which case there is no header
         //since it can't be null or any other class.
         if (inheritFromClass.isPrimitive()) return new HeaderInformation(ClassUtil.boxClass(inheritFromClass).getName());
         //can't ignore header if inheritFromClass is final because it could be null (thus component will be either '?' or ';')
         firstByte = reader.readByte();
         dimensionCount = ArrayUtil.countArrayDimensions(inheritFromClass);
         final Class<?> baseComponent = inheritFromClass.isArray() ? ArrayUtil.getBaseComponentType(inheritFromClass) : inheritFromClass;
         primitiveArray = baseComponent.isPrimitive();
         if ('?' == firstByte)
         {
            return new HeaderInformation(baseComponent.getName(), dimensionCount, primitiveArray);
         }
         //if inheritFromClass isn't primitive then it is not required to inherit type (eg null or child class) and continues below
      }
      else
      {
         firstByte = reader.readByte();
         if ('?' == firstByte) throw new StreamCorruptedException("Only array elements can inherit type");
         primitiveArray = (']' == firstByte);  //is false if not an array at all
         if ('[' == firstByte || ']' == firstByte)
         {
            if (reader.remainingBytes() == 0) throw new StreamCorruptedException("Incomplete header: no array dimensions");
            dimensionCount = Byte.toUnsignedInt(reader.readByte());
            if (reader.remainingBytes() == 0) throw new StreamCorruptedException("Incomplete header: no array component type");
            firstByte = reader.readByte();
            if (';' == firstByte) throw new StreamCorruptedException("header's array component type can't be null");
            if ('-' == firstByte) throw new StreamCorruptedException("header's array component type can't be false");
            if ('+' == firstByte) return new HeaderInformation(Boolean.class.getName(), dimensionCount, primitiveArray);
         }
         else dimensionCount = 0;
      }

      if (';' == firstByte) return new HeaderInformation();  //the empty string class name means null
      if ('+' == firstByte) return new HeaderInformation(Boolean.TRUE);
      if ('-' == firstByte) return new HeaderInformation(Boolean.FALSE);
      if (COMPRESSED_HEADER_TO_CLASS.containsKey((char) firstByte))  //safe cast because map contains only ASCII
      {
         final Class<?> compressedClass = COMPRESSED_HEADER_TO_CLASS.get((char) firstByte);  //safe cast because map contains only ASCII
         return new HeaderInformation(compressedClass.getName(), dimensionCount, primitiveArray);
      }

      //else firstByte is part of a class name
      return new HeaderInformation(StringSerializableStrategy.readClassName(reader, firstByte), dimensionCount, primitiveArray);
   }

   public static void writeHeader(final AsynchronousFileAppender appender, final Class<?> inheritFromClass, final Object data)
   {
      //boolean[] and Boolean[] use only headers for elements (primitive doesn't allow null)
      if (Boolean.TRUE.equals(data)) writeByte(appender, '+');
      else if (Boolean.FALSE.equals(data)) writeByte(appender, '-');
      else if (data == null) writeByte(appender, ';');  //if data is null then class name is the empty string
      else if (null != inheritFromClass && inheritFromClass.isPrimitive()) ;  //do nothing
         //because non-boolean primitive array elements have no header
         //(below) if class matches containing array exactly then inherit type.
      else if (null != inheritFromClass && inheritFromClass.equals(data.getClass())) writeByte(appender, '?');
      else if (CLASS_TO_COMPRESSED_HEADER.containsKey(data.getClass()))
         writeByte(appender, CLASS_TO_COMPRESSED_HEADER.get(data.getClass()));
      else if (data.getClass().isArray())
      {
         Class<?> baseComponent = ArrayUtil.getBaseComponentType(data.getClass());
         if (Object.class.equals(inheritFromClass) || null == inheritFromClass)
         {
            //array indicator and dimension count can be derived from containing array so don't populate it
            //unless I'm inside Object[] in which case I could have new arrays.
            //baseComponent can't be void or null
            if (baseComponent.isPrimitive())
            {
               writeByte(appender, ']');
               baseComponent = ClassUtil.boxClass(baseComponent);
            }
            else writeByte(appender, '[');

            final int dimensionCount = ArrayUtil.countArrayDimensions(data.getClass());
            writeByte(appender, dimensionCount);  //won't be 0, max: 255. Use unsigned byte
         }

         if (baseComponent.equals(Boolean.class)) writeByte(appender, '+');
         else if (CLASS_TO_COMPRESSED_HEADER.containsKey(baseComponent)) writeByte(appender, CLASS_TO_COMPRESSED_HEADER.get(baseComponent));
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
