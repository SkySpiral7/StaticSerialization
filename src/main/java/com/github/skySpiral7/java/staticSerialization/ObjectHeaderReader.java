package com.github.skySpiral7.java.staticSerialization;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import com.github.skySpiral7.java.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;

public enum ObjectHeaderReader
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
   private static final Map<Character, Class<?>> COMPRESSED_CLASSES;

   static
   {
      COMPRESSED_CLASSES = new HashMap<>();
      COMPRESSED_CLASSES.put('~', Byte.class);
      COMPRESSED_CLASSES.put('!', Short.class);
      COMPRESSED_CLASSES.put('@', Integer.class);
      COMPRESSED_CLASSES.put('#', Long.class);
      COMPRESSED_CLASSES.put('%', Float.class);
      COMPRESSED_CLASSES.put('^', Double.class);
      COMPRESSED_CLASSES.put('&', Character.class);
      COMPRESSED_CLASSES.put('*', String.class);
   }

   public static HeaderInformation readOverhead(final AsynchronousFileReader reader)
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
      if (COMPRESSED_CLASSES.containsKey((char) firstByte))
      {
         final Class<?> compressedClass = COMPRESSED_CLASSES.get((char) firstByte);
         return new HeaderInformation(compressedClass.getName(), dimensionCount);
      }

      //else firstByte is part of a class name
      final ByteArrayOutputStream classNameStream = new ByteArrayOutputStream();
      classNameStream.write(firstByte);
      while (true)
      {
         if (reader.remainingBytes() == 0) throw new StreamCorruptedException("Incomplete header: class name not terminated");
         final byte thisByte = reader.readByte();
         if (thisByte == ';') break;
         classNameStream.write(thisByte);
      }
      final String actualClassName = new String(classNameStream.toByteArray(), StandardCharsets.UTF_8);
      return new HeaderInformation(actualClassName, dimensionCount);
   }
}
