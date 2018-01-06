package com.github.skySpiral7.java.staticSerialization;

import java.io.ByteArrayInputStream;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.List;
import java.util.Objects;

import com.github.skySpiral7.java.AsynchronousFileReader;
import com.github.skySpiral7.java.exception.NoMoreDataException;
import com.github.skySpiral7.java.staticSerialization.exception.DeserializationException;
import com.github.skySpiral7.java.staticSerialization.exception.InvalidClassException;
import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.IntegerSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.StringSerializableStrategy;
import com.github.skySpiral7.java.util.ArrayUtil;
import com.github.skySpiral7.java.util.BitWiseUtil;
import com.github.skySpiral7.java.util.ClassUtil;

import static com.github.skySpiral7.java.util.ClassUtil.cast;

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
   public void close(){fileReader.close();}

   public boolean hasData(){return fileReader.hasData();}

   public int remainingBytes(){return fileReader.remainingBytes();}

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

      final HeaderInformation headerInformation = HeaderSerializableStrategy.readOverhead(fileReader);
      if (headerInformation.getClassName() == null) return null;  //can be cast to anything safely
      //TODO: test
      if (headerInformation.getDimensionCount() > 1) throw new UnsupportedOperationException("Currently only 1d arrays are supported");
      if (headerInformation.getDimensionCount() == 0 && Boolean.class.getName().equals(headerInformation.getClassName()))
      {
         if (!allowChildClass && !Boolean.class.equals(expectedClass)) throw new IllegalStateException(
               "Class doesn't match exactly. Expected: " + expectedClass.getName() + " Got: java.lang.Boolean");
         //TODO: I think this is redundant?
         if (!expectedClass.isAssignableFrom(Boolean.class))
            //Same message as JVM.
            throw new ClassCastException(Boolean.class.getName() + " cannot be cast to " + expectedClass.getName());
         if (headerInformation.getValue() != null) return cast(headerInformation.getValue());  //either true or false
         //will be null if the header explicitly contained Boolean for some reason in which case will be read below
      }

      final Class<T_Actual> actualClass = getClassFromOverhead(headerInformation, expectedClass, allowChildClass);
      return readValue(actualClass, headerInformation.getDimensionCount());
   }

   private <T> T readValue(final Class<T> actualClass, final int dimensionCount)
   {
      if (0 == dimensionCount) return readNonArrayValue(actualClass);
      final int arrayLength = IntegerSerializableStrategy.read(fileReader);
      final Object arrayValue = Array.newInstance(actualClass.getComponentType(), arrayLength);
      for (int readIndex = 0; readIndex < arrayLength; ++readIndex)
      {
         Array.set(arrayValue, readIndex, readObject(actualClass.getComponentType()));
      }
      return cast(arrayValue);
   }

   private <T> T readNonArrayValue(final Class<T> actualClass)
   {
      if (ClassUtil.isBoxedPrimitive(actualClass)) return readPrimitive(actualClass);
      if (String.class.equals(actualClass))
      {
         return cast(StringSerializableStrategy.readWithLength(fileReader));
      }

      if (StaticSerializable.class.isAssignableFrom(actualClass)){ return readCustomClass(actualClass); }

      if (actualClass.isEnum()){ return readEnumByOrdinal(actualClass); }
      if (Serializable.class.isAssignableFrom(actualClass))
      {
         final int length = IntegerSerializableStrategy.read(fileReader);
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

   private <T_Expected, T_Actual extends T_Expected> Class<T_Actual> getClassFromOverhead(final HeaderInformation actualHeader,
                                                                                          final Class<T_Expected> expectedClass,
                                                                                          final boolean allowChildClass)
   {
      final int expectedDimensions = ArrayUtil.countArrayDimensions(expectedClass);
      final Class<?> expectedBaseComponentType = ArrayUtil.getBaseComponentType(expectedClass);
      if (!allowChildClass)
      {
         if (expectedClass.isArray())
         {
            final HeaderInformation expectedHeader = new HeaderInformation(expectedBaseComponentType.getName(), expectedDimensions);
            if (0 == actualHeader.getDimensionCount()) throw new IllegalStateException(
                  "Class doesn't match exactly. Expected: " + expectedHeader + " Got: " + actualHeader.getClassName());
            if (!expectedHeader.equals(actualHeader))
               throw new IllegalStateException("Class doesn't match exactly. Expected: " + expectedHeader + " Got: " + actualHeader);
         }
         else
         {
            if (0 != actualHeader.getDimensionCount()) throw new IllegalStateException(
                  "Class doesn't match exactly. Expected: " + expectedClass.getName() + " Got: " + actualHeader);
            if (!expectedClass.getName().equals(actualHeader.getClassName())) throw new IllegalStateException(
                  "Class doesn't match exactly. Expected: " + expectedClass.getName() + " Got: " + actualHeader.getClassName());
         }
      }
      //it is important to validate here so that some nefarious static initialization blocks won't be ran
      //if casting is allowed then loading the class is unavoidable at this point

      final Class<?> actualClass;
      try
      {
         actualClass = Class.forName(actualHeader.getClassName());
      }
      catch (final ClassNotFoundException classNotFoundException)
      {
         throw new DeserializationException(classNotFoundException);
      }
      if (expectedClass.isArray())
      {
         if (!expectedBaseComponentType.isAssignableFrom(actualClass))
            //Not redundant because this is the only check for empty arrays
            //and checking here is better than waiting for failing to set an element in the array.
            //Same message as JVM.
            throw new ClassCastException(actualClass.getName() + " cannot be cast to " + expectedBaseComponentType.getName());
         final int[] arrayOfLengths = new int[expectedDimensions];  //they are filled with 0 by default
         //It is easier to create an empty array then a string that would match the class name.
         return cast(Array.newInstance(expectedBaseComponentType, arrayOfLengths).getClass());
      }
      if (!expectedClass.isAssignableFrom(actualClass))
         //Not redundant because it needs to fail based on expectedClass after type erasure before getting to the client.
         //Same message as JVM.
         //TODO: make sure that this throws when actualClass is primitive void
         throw new ClassCastException(actualClass.getName() + " cannot be cast to " + expectedClass.getName());
      return cast(actualClass);
   }

   private <T> T readPrimitive(final Class<T> expectedClass)
   {
      if (Byte.class.equals(expectedClass)) return cast(fileReader.readByte());
      if (Short.class.equals(expectedClass))
      {
         final byte[] data = fileReader.readBytes(2);
         final int result = ((data[0] & 0xff) << 8) | (data[1] & 0xff);
         return cast((short) result);
      }
      if (Integer.class.equals(expectedClass))
      {
         return cast(IntegerSerializableStrategy.read(fileReader));
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
         final byte data = fileReader.readByte();
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
      final int ordinal = IntegerSerializableStrategy.read(fileReader);
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
      allSerializableFields.forEach(field -> {
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
