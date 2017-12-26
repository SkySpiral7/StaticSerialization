package com.github.skySpiral7.java.staticSerialization;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import com.github.skySpiral7.java.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.exception.NoMoreDataException;

public enum ObjectHeaderReader
{
   ;  //no instances

/*
[x where x is unsigned byte which is the number of dimensions (JVM max is 255)
[1~ is easy just the length (int) then elements
Even though elements will be serialized as primitives do not change [1java.lang.Byte; to [1~
   because the resulting array type will be different
[2~ length. each one needs overhead since arrays maintain the component type ([2~ length has [1~ length)
   however if the base type is primitive then it can be only those in which case:
   length of root, length, length, length, flat data:
   [
   [1,2],
   [1,2,3],
   [1]
   ] => 3, 2,3,1, 1,2,1,2,3,1
   sounds hard to manage for arbitrary depth
   works for any final class but no reason I can't use normal order:
   ~ => 3, 2,1,2, 3,1,2,3, 1,1
[2java.lang.Object; length 2 contains [1java.lang.Byte; and [1java.lang.Double;
   not the same as primitive arrays but will serialize elements as primitive
   true for any child array class
   if held on to an indicator I could convert [1java.lang.Byte; => [1~ since Object[] can't have int[]
   example: Object[Byte[2,3], Integer[4,5]] becomes
   [2java.lang.Object;2[1java.lang.Byte;223[1java.lang.Integer;200040005
   only include dimensions and array indicator for the top because the rest is assumed
   heck if the class is final (eg primitive) I can even assume the component type
How does strict work? Only the top level wouldn't be safe enough
   passing in an already created array would be hard for clients and wouldn't protect from Object elements
   strict could enforce homogeneous array although not desirable
   for now: strict doesn't support arrays at all therefore the only safe array is one with a final class base component
*/

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
      Objects.nonNull(reader);
      if (reader.remainingBytes() == 0) throw new NoMoreDataException();

      final byte firstByte = reader.readByte();
      //the empty string class name means null
      if (';' == firstByte) return new HeaderInformation(null);
      if ('+' == firstByte) return new HeaderInformation(Boolean.class.getName(), Boolean.TRUE);
      if ('-' == firstByte) return new HeaderInformation(Boolean.class.getName(), Boolean.FALSE);
      if (COMPRESSED_CLASSES.containsKey((char) firstByte))
      {
         final Class<?> compressedClass = COMPRESSED_CLASSES.get((char) firstByte);
         return new HeaderInformation(compressedClass.getName());
      }
      //         if ('[' == firstByte)
      //         {
      //            final byte dimensions = fileReader.readByte();
      //            final Class<?> componentType = readObjectInternal(expectedClass, allowChildClass, assumeDimensions);
      //            for()
      //            {
      //               readObjectInternal(Object[][].class, true);
      //            }
      //         }

      //else firstByte is part of a class name
      final ByteArrayOutputStream data = new ByteArrayOutputStream();
      data.write(firstByte);
      while (true)
      {
         if (reader.remainingBytes() == 0) throw new StreamCorruptedException("Incomplete header");
         final byte thisByte = reader.readByte();
         if (thisByte == ';') break;
         data.write(thisByte);
      }
      final String actualClassName = new String(data.toByteArray(), StandardCharsets.UTF_8);
      return new HeaderInformation(actualClassName);
   }
}
