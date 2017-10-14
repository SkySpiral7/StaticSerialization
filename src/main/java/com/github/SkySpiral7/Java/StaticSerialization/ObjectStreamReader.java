package com.github.SkySpiral7.Java.StaticSerialization;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.github.SkySpiral7.Java.AsynchronousFileReader;
import com.github.SkySpiral7.Java.exception.NoMoreDataException;
import com.github.SkySpiral7.Java.util.ClassUtil;

import static com.github.SkySpiral7.Java.util.ClassUtil.cast;

public class ObjectStreamReader implements Closeable
{
   private final ObjectReaderRegistry registry = new ObjectReaderRegistry();
   private final AsynchronousFileReader fileReader;

/*
[x where x is unsigned byte which is the number of dimensions (JVM max is 255)
[1~ is easy just the length (int) then elements
Even though elements will be serialized as primitives do not change [1java.lang.Byte| to [1~
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
   works for any final class
[2java.lang.Object| length 2 contains [1java.lang.Byte| and [1java.lang.Double|
   not the same as primitive arrays but will serialize elements as primitive
   true for any child array class
   if held on to an indicator I could convert [1java.lang.Byte| => [1~ since Object[] can't have int[]
   example: Object[Byte[2,3], Integer[4,5]] becomes
   [2java.lang.Object|2[1java.lang.Byte|223[1java.lang.Integer|200040005
*/
   /**
    * Not in map:<br/>
    * + boolean true<br/>
    * - boolean false<br/>
    * [2<br/>
    * | null<br/>
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

   public ObjectStreamReader(final File sourceFile)
   {
      fileReader = new AsynchronousFileReader(sourceFile);
   }

   /**
    * @see AsynchronousFileReader#close()
    */
   @Override
   public void close()
   {
      fileReader.close();
   }

   //hasData(int) would be nice but would need to read and store the overhead
   public boolean hasData()
   {
      return (remainingBytes() > 0);
   }

   public int remainingBytes()
   {
      return fileReader.remainingBytes();
   }

   /**
    * Reads the next object in the stream no matter what it is.
    * For security this means that you either trust the stream or you trust all available classes.
    *
    * @see #readObject(Class)
    */
   public <T> T readObject()
   {
      return cast(readObject(Object.class));
   }

   /**
    * Reads an object from the stream and requires that the class must match exactly. While normally
    * you could just call the class's readFromStream method, this method is useful if either the class
    * implements Serializable rather than StaticSerializable (such as BigDecimal), or if you don't know the
    * exact class at compile time and would like this method to do the reflection for you.
    *
    * @see #readObject(Class)
    */
   public <T> T readObjectStrictly(Class<T> expectedClass)
   {
      return readObjectInternal(expectedClass, false);
   }

   /* TODO: unfinished doc
    * @throws ClassNotFoundException
    *       if the class indicated by the stream doesn't exist
    * @throws IOException
    *       only thrown when Java's deserialization is used and "Any of the usual Input/Output related exceptions." occurs.
    * @see ObjectInputStream#readObject()
    */
   public <T> T readObject(Class<T> expectedClass)
   {
      return readObjectInternal(expectedClass, true);
   }

   private <T> T readObjectInternal(Class<T> expectedClass, final boolean allowChildClass)
   {
      Objects.requireNonNull(expectedClass);
      if (!hasData()) throw new NoMoreDataException();

      //must check for void.class because ClassUtil.boxClass would throw something less helpful
      if (void.class.equals(expectedClass)) throw new IllegalArgumentException("There are no instances of void");
      if (expectedClass.isPrimitive()) expectedClass = cast(ClassUtil.boxClass(expectedClass));
      //Class Overhead
      {
         final byte firstByte = fileReader.readBytes(1)[0];
         if ('|' == firstByte) return null;  //the empty string class name means null, which can be cast to anything safely
         if (('+' == firstByte || '-' == firstByte) && !allowChildClass && !Boolean.class.equals(expectedClass))
            throw new IllegalStateException(
                  "Class doesn't match exactly. Expected: " + expectedClass.getName() + " Got: java.lang.Boolean");
         if ('+' == firstByte) return cast(Boolean.TRUE);
         if ('-' == firstByte) return cast(Boolean.FALSE);
         //TODO: for now it doesn't allow array
         if ('[' == firstByte) throw new UnsupportedOperationException("Arrays are not currently supported");
         expectedClass = cast(readOverhead(expectedClass, firstByte, allowChildClass));
      }

      if (ClassUtil.isBoxedPrimitive(expectedClass)) return readPrimitive(expectedClass);
      if (String.class.equals(expectedClass))
      {
         final int stringByteLength = BitWiseUtil.bigEndianBytesToInteger(fileReader.readBytes(4));
         return cast(fileReader.readBytesAsString(stringByteLength));
      }

      if (StaticSerializable.class.isAssignableFrom(expectedClass)){ return readCustomClass(expectedClass); }

      if (expectedClass.isEnum()){ return readEnumByOrdinal(expectedClass); }
      if (Serializable.class.isAssignableFrom(expectedClass))
      {
         final int length = BitWiseUtil.bigEndianBytesToInteger(fileReader.readBytes(4));
         final byte[] objectData = fileReader.readBytes(length);
         return javaDeserialize(objectData);
      }

      throw new NotSerializableException(expectedClass);
   }

   private <T> T javaDeserialize(final byte[] objectData)
   {
      final ByteArrayInputStream byteStream = new ByteArrayInputStream(objectData);
      try (final ObjectInputStream in = new ObjectInputStream(byteStream))
      {
         return cast(in.readObject());
      }
      catch (final ObjectStreamException ex)
      {
         throw new StreamCorruptedException(ex);
      }
      catch (final ClassNotFoundException | IOException e)
      {
         throw new DeserializationException(e);
      }
   }

   private Class<?> readOverhead(final Class<?> expectedClass, final byte firstByte, final boolean allowChildClass)
   {
      if (COMPRESSED_CLASSES.containsKey((char) firstByte))
      {
         final Class<?> actualClass = COMPRESSED_CLASSES.get((char) firstByte);
         if (!allowChildClass && !actualClass.equals(expectedClass)) throw new IllegalStateException(
               "Class doesn't match exactly. Expected: " + expectedClass.getName() + " Got: " + actualClass.getName());
         return actualClass;
      }
      final ByteArrayOutputStream data = new ByteArrayOutputStream();
      data.write(firstByte);
      while (true)
      {
         if (!hasData()) throw new StreamCorruptedException("Incomplete header");
         final byte thisByte = fileReader.readBytes(1)[0];
         if (thisByte == '|') break;
         data.write(thisByte);
      }
      final String actualClassName = new String(data.toByteArray(), StandardCharsets.UTF_8);
      if (!allowChildClass && !actualClassName.equals(expectedClass.getName()))
         throw new IllegalStateException("Class doesn't match exactly. Expected: " + expectedClass.getName() + " Got: " + actualClassName);

      try
      {
         Class<?> actualClass = Class.forName(actualClassName);
         if (!expectedClass.isAssignableFrom(actualClass))
            throw new ClassCastException(actualClass.getName() + " can't be cast into " + expectedClass.getName());
         return actualClass;
      }
      catch (final ClassNotFoundException e)
      {
         throw new DeserializationException(e);
      }
   }

   private <T> T readPrimitive(final Class<T> expectedClass)
   {
      if (Byte.class.equals(expectedClass)) return cast(fileReader.readBytes(1)[0]);
      if (Short.class.equals(expectedClass))
      {
         final byte[] data = fileReader.readBytes(2);
         final int result = ((data[0] & 0xff) << 8) | (data[1] & 0xff);
         return cast((short) result);
      }
      if (Integer.class.equals(expectedClass))
      {
         final byte[] data = fileReader.readBytes(4);
         return cast(BitWiseUtil.bigEndianBytesToInteger(data));
      }
      if (Long.class.equals(expectedClass))
      {
         final byte[] data = fileReader.readBytes(8);
         return cast(BitWiseUtil.bigEndianBytesToLong(data));
      }
      if (Float.class.equals(expectedClass))
      {
         final byte[] data = fileReader.readBytes(4);
         final int intData = BitWiseUtil.bigEndianBytesToInteger(data);
         return cast(Float.intBitsToFloat(intData));
      }
      if (Double.class.equals(expectedClass))
      {
         final byte[] data = fileReader.readBytes(8);
         final long longData = BitWiseUtil.bigEndianBytesToLong(data);
         return cast(Double.longBitsToDouble(longData));
      }
      if (Boolean.class.equals(expectedClass))
      {
         //This code is obsolete but still permitted. It isn't normally reached due to the new boolean overhead.
         final byte data = fileReader.readBytes(1)[0];
         if (data == 1) return cast(Boolean.TRUE);
         return cast(Boolean.FALSE);
      }
      if (Character.class.equals(expectedClass))
      {
         final byte[] data = fileReader.readBytes(2);
         final int intData = ((data[0] & 0xff) << 8) | (data[1] & 0xff);
         //TODO: either prove that casting to short isn't needed or write a test to enforce it
         return cast((char) (short) intData);
      }

      throw new AssertionError("Method shouldn't've been called");
   }

   private <T> T readEnumByOrdinal(final Class<T> expectedClass)
   {
      final int ordinal = readObject(int.class);
      final Enum<?>[] values = Enum[].class.cast(expectedClass.getEnumConstants());  //won't return null because it is an enum

      if (values.length <= ordinal) throw new StreamCorruptedException(
            String.format("%s.values()[%d] doesn't exist. Actual length: %d", expectedClass.getName(), ordinal, values.length));

      return cast(values[ordinal]);
   }

   private <T> T readCustomClass(final Class<T> expectedClass)
   {
      final Method method;
      try
      {
         //public static T readFromStream(ObjectStreamReader reader)
         method = expectedClass.getDeclaredMethod("readFromStream", ObjectStreamReader.class);
      }
      catch (final NoSuchMethodException e)
      {
         throw new InvalidClassException(expectedClass.getName() + " implements StaticSerializable but doesn't define readFromStream");
      }

      if (!Modifier.isPublic(method.getModifiers()) || !Modifier.isStatic(method.getModifiers()))
      {
         throw new InvalidClassException(expectedClass.getName() + ".readFromStream must be public static");
      }

      try
      {
         return cast(method.invoke(null, this));
      }
      catch (final IllegalAccessException | IllegalArgumentException e)
      {
         throw new AssertionError("This can't be thrown", e);
         //since I already know it is public static and I know I'm giving it the right args
         //(because otherwise it wouldn't have been found)
      }
      catch (final InvocationTargetException e)
      {
         throw new DeserializationException(e);
      }
   }

   public void readFieldsReflectively(final Object instance)
   {
      final List<Field> allSerializableFields = SerializationUtil.getAllSerializableFields(instance.getClass());
      allSerializableFields.forEach(field ->
      {
         field.setAccessible(true);
         try
         {
            field.set(instance, this.readObject());  //will auto-cast
         }
         catch (final IllegalAccessException e)
         {
            throw new AssertionError("This can't be thrown.", e);
            //since I would've gotten SecurityException from setAccessible(true)
         }
      });
   }

   public ObjectReaderRegistry getObjectRegistry()
   {
      return registry;
   }
}
