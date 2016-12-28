package com.github.SkySpiral7.Java.serialization;

import java.io.*;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;

import com.github.SkySpiral7.Java.AsynchronousFileReader;
import com.github.SkySpiral7.Java.exception.*;
import com.github.SkySpiral7.Java.exception.InvalidClassException;
import com.github.SkySpiral7.Java.exception.NotSerializableException;
import com.github.SkySpiral7.Java.exception.StreamCorruptedException;
import com.github.SkySpiral7.Java.util.BitWiseUtil;
import com.github.SkySpiral7.Java.util.ClassUtil;

public class ObjectStreamReader implements Closeable
{
   private final ObjectReaderRegistry registry = new ObjectReaderRegistry();
   private final AsynchronousFileReader fileReader;

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

   public boolean hasData()
   {
      return hasData(1);
   }

   //TODO: this is misleading because it includes overhead
   public boolean hasData(final int byteCount)
   {
      return (byteCount <= remainingBytes());
   }

   public int remainingBytes()
   {
      return fileReader.remainingBytes();
   }

   public Object readObject()
   {
      return readObject(Object.class);
   }

   /* TODO: unfinished doc
    * @throws ClassNotFoundException
    *       if the class indicated by the stream doesn't exist
    * @throws IOException
    *       only thrown when Java's deserialization is used and "Any of the usual Input/Output related exceptions." occurs.
    * @see ObjectInputStream#readObject()
    */
   @SuppressWarnings("unchecked")
   public <T> T readObject(Class<T> expectedClass)
   {
      Objects.requireNonNull(expectedClass);
      if (!hasData()) throw new NoMoreDataException();

      //must check for void.class because ClassUtil.boxClass would throw something less helpful
      if (void.class.equals(expectedClass)) throw new IllegalArgumentException("There are no instances of void");
      if (expectedClass.isPrimitive()) expectedClass = (Class<T>) ClassUtil.boxClass(expectedClass);
      //Class Overhead
      {
         final byte firstByte = fileReader.readBytes(1)[0];
         if (firstByte == '|') return null;  //the empty string class name means null, which can be cast to anything
         expectedClass = (Class<T>) readOverhead(expectedClass, firstByte);
      }
      //TODO: for now it doesn't allow array

      if (ClassUtil.isBoxedPrimitive(expectedClass)) return readPrimitive(expectedClass);
      if (expectedClass.isAnnotationPresent(GenerateId.class))
      {
         final T registeredObject = registry.readObjectOrId(this);
         if (registeredObject != null) return registeredObject;
      }

      if (String.class.equals(expectedClass))
      {
         final int stringByteLength = BitWiseUtil.bigEndianBytesToInteger(fileReader.readBytes(4));
         return (T) fileReader.readBytesAsString(stringByteLength);
      }

      if (StaticSerializableEnumByName.class.isAssignableFrom(expectedClass))
      {
         final String name = readObject(String.class);
         return (T) Enum.valueOf(ClassUtil.cast(expectedClass), name);
      }
      if (StaticSerializableEnumByOrdinal.class.isAssignableFrom(expectedClass)) { return readEnumByOrdinal(expectedClass); }

      if (StaticSerializable.class.isAssignableFrom(expectedClass)) { return readCustomClass(expectedClass); }
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
         return ClassUtil.cast(in.readObject());
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

   private Class<?> readOverhead(final Class<?> expectedClass, final byte firstByte)
   {
      final String actualClassName;

      if ('&' == firstByte)
      {
         final char classCode = (char) fileReader.readBytes(1)[0];
         if ('T' == classCode) actualClassName = "java.lang.String";
         else actualClassName = "[" + classCode;  //classCode needs to be decoded as an array by java
      }
      else
      {
         final ByteArrayOutputStream data = new ByteArrayOutputStream();
         data.write(firstByte);
         while (true)
         {
            if (!hasData()) throw new StreamCorruptedException("Header not found");
            final byte thisByte = fileReader.readBytes(1)[0];
            if (thisByte == '|') break;
            data.write(thisByte);
         }
         actualClassName = new String(data.toByteArray(), StandardCharsets.UTF_8);
      }

      try
      {
         Class<?> actualClass = Class.forName(actualClassName);
         if ('&' == firstByte && !"java.lang.String".equals(actualClassName))
         {
            actualClass = actualClass.getComponentType();  //it was wrapped in an array in order to decode the value
            actualClass = ClassUtil.boxClass(actualClass);  //expectedClass has already been boxed
         }
         if (!expectedClass.isAssignableFrom(actualClass)) throw new ClassCastException(actualClass.getName()
                                                                                        + " can't be cast into " + expectedClass.getName());
         return actualClass;
      }
      catch (final ClassNotFoundException e)
      {
         throw new DeserializationException(e);
      }
   }

   @SuppressWarnings("unchecked")
   private <T> T readPrimitive(final Class<T> expectedClass)
   {
      if (Byte.class.equals(expectedClass)) return (T) (Byte) fileReader.readBytes(1)[0];
      if (Short.class.equals(expectedClass))
      {
         final byte[] data = fileReader.readBytes(2);
         final int result = ((data[0] & 0xff) << 8) | (data[1] & 0xff);
         return (T) (Short) (short) result;
      }
      if (Integer.class.equals(expectedClass))
      {
         final byte[] data = fileReader.readBytes(4);
         return (T) (Integer) BitWiseUtil.bigEndianBytesToInteger(data);
      }
      if (Long.class.equals(expectedClass))
      {
         final byte[] data = fileReader.readBytes(8);
         return (T) (Long) BitWiseUtil.bigEndianBytesToLong(data);
      }
      if (Float.class.equals(expectedClass))
      {
         final byte[] data = fileReader.readBytes(4);
         final int intData = BitWiseUtil.bigEndianBytesToInteger(data);
         return (T) (Float) Float.intBitsToFloat(intData);
      }
      if (Double.class.equals(expectedClass))
      {
         final byte[] data = fileReader.readBytes(8);
         final long longData = BitWiseUtil.bigEndianBytesToLong(data);
         return (T) (Double) Double.longBitsToDouble(longData);
      }
      if (Boolean.class.equals(expectedClass))
      {
         final byte data = fileReader.readBytes(1)[0];
         if (data == 1) return (T) Boolean.TRUE;
         return (T) Boolean.FALSE;
      }
      if (Character.class.equals(expectedClass))
      {
         final byte[] data = fileReader.readBytes(2);
         final int intData = ((data[0] & 0xff) << 8) | (data[1] & 0xff);
         return (T) (Character) (char) (short) intData;
      }

      throw new AssertionError("Method shouldn't've been called");
   }

   private <T> T readEnumByOrdinal(final Class<T> expectedClass)
   {
      if (!expectedClass.isEnum()) throw new InvalidClassException(expectedClass.getName() + " implements StaticSerializableEnumByOrdinal but isn't an enum");

      final int ordinal = readObject(int.class);
      final Enum<?>[] values = Enum[].class.cast(expectedClass.getEnumConstants());  //won't return null because it is an enum

      if (values.length <= ordinal) throw new StreamCorruptedException(String.format(
            "%s[%d] doesn't exist. Actual length: %d", expectedClass.getName(), ordinal, values.length));

      return ClassUtil.cast(values[ordinal]);
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
         return ClassUtil.cast(method.invoke(null, this));
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
