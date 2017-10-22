package com.github.SkySpiral7.Java.StaticSerialization;

import java.io.ByteArrayInputStream;
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
import java.util.List;
import java.util.Objects;

import com.github.SkySpiral7.Java.AsynchronousFileReader;
import com.github.SkySpiral7.Java.exception.NoMoreDataException;
import com.github.SkySpiral7.Java.util.ClassUtil;

import static com.github.SkySpiral7.Java.StaticSerialization.ClassUtil.cast;

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

   //hasData(int) would be nice but would need to read and store the overhead
   public boolean hasData()
   {
      //TODO: move hasData to AsynchronousFileReader
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
    * <p>Reads an object from the stream and requires that the class must match exactly. While normally
    * you could just call the class's readFromStream method, this method is useful if either the class
    * implements Serializable rather than StaticSerializable (such as BigDecimal), or if you don't know the
    * exact class at compile time and would like this method to do the reflection for you.</p>
    *
    * <p>Security feature: if the expected class isn't the same as the class in this stream then an IllegalStateException
    * is thrown without loading the class found. Thus untrusted classes will not be loaded (preventing static initializer
    * blocks).</p>
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

   /**
    * @param allowChildClass true will throw if the class found isn't the exact same. false allows casting.
    */
   private <T_Expected, T_Actual extends T_Expected> T_Actual readObjectInternal(Class<T_Expected> expectedClass,
                                                                                 final boolean allowChildClass)
   {
      Objects.requireNonNull(expectedClass);
      if (!hasData()) throw new NoMoreDataException();

      //must check for void.class because ClassUtil.boxClass would throw something less helpful
      if (void.class.equals(expectedClass)) throw new IllegalArgumentException("There are no instances of void");
      if (expectedClass.isPrimitive()) expectedClass = cast(ClassUtil.boxClass(expectedClass));

      final HeaderInformation headerInformation = ObjectHeaderReader.readOverhead(fileReader);
      if(headerInformation.getClassName() == null) return null;  //can be cast to anything safely
      if (Boolean.class.getName().equals(headerInformation.getClassName()))
      {
         if (!allowChildClass && !Boolean.class.equals(expectedClass)) throw new IllegalStateException(
               "Class doesn't match exactly. Expected: " + expectedClass.getName() + " Got: java.lang.Boolean");
         //TODO: I think this is redundant?
         if (!expectedClass.isAssignableFrom(Boolean.class))
            throw new ClassCastException(Boolean.class.getName() + " can't be cast into " + expectedClass.getName());
         if(headerInformation.getValue() != null) return cast(headerInformation.getValue());  //either true or false
         //will be null if the header explicitly contained Boolean for some reason in which case will be read below
      }

      final Class<T_Actual> actualClass = getClassFromOverhead(headerInformation.getClassName(), expectedClass, allowChildClass);
      return readValue(actualClass);
   }

   private <T> T readValue(final Class<T> actualClass)
   {
      if (ClassUtil.isBoxedPrimitive(actualClass)) return readPrimitive(actualClass);
      if (String.class.equals(actualClass))
      {
         final int stringByteLength = BitWiseUtil.bigEndianBytesToInteger(fileReader.readBytes(4));
         return cast(fileReader.readBytesAsString(stringByteLength));
      }

      if (StaticSerializable.class.isAssignableFrom(actualClass)){ return readCustomClass(actualClass); }

      if (actualClass.isEnum()){ return readEnumByOrdinal(actualClass); }
      if (Serializable.class.isAssignableFrom(actualClass))
      {
         final int length = BitWiseUtil.bigEndianBytesToInteger(fileReader.readBytes(4));
         final byte[] objectData = fileReader.readBytes(length);
         return javaDeserialize(objectData);
      }

      throw new NotSerializableException(actualClass);
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

   private <T_Expected, T_Actual extends T_Expected> Class<T_Actual> getClassFromOverhead(final String actualClassName,
                                                                                          final Class<T_Expected> expectedClass,
                                                                                          final boolean allowChildClass)
   {
      if (!allowChildClass && !actualClassName.equals(expectedClass.getName()))
         throw new IllegalStateException("Class doesn't match exactly. Expected: " + expectedClass.getName() + " Got: " + actualClassName);
      //it is important to validate here so that some nefarious static blocks won't be ran
      //if casting is allowed then loading the class is unavoidable at this point

      try
      {
         final Class<?> actualClass = Class.forName(actualClassName);
         if (!expectedClass.isAssignableFrom(actualClass))
            throw new ClassCastException(actualClass.getName() + " can't be cast into " + expectedClass.getName());
         return cast(actualClass);
      }
      catch (final ClassNotFoundException classNotFoundException)
      {
         throw new DeserializationException(classNotFoundException);
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
