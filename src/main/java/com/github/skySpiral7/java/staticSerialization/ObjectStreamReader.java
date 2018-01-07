package com.github.skySpiral7.java.staticSerialization;

import java.io.Closeable;
import java.io.File;
import java.util.Objects;

import com.github.skySpiral7.java.AsynchronousFileReader;
import com.github.skySpiral7.java.exception.NoMoreDataException;
import com.github.skySpiral7.java.staticSerialization.strategy.AllSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ReaderValidationStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ReflectionSerializableStrategy;
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

      final HeaderInformation headerInformation = HeaderSerializableStrategy.readHeader(fileReader);
      if (headerInformation.getClassName() == null) return null;  //can be cast to anything safely
      if (headerInformation.getDimensionCount() == 0 && Boolean.class.getName().equals(headerInformation.getClassName()))
      {
         ReaderValidationStrategy.validateBoolean(expectedClass, allowChildClass);
         if (headerInformation.getValue() != null) return cast(headerInformation.getValue());  //either true or false
         //will be null if the header explicitly contained Boolean for some reason in which case will be read below
      }

      final Class<T_Actual> actualClass = ReaderValidationStrategy.getClassFromHeader(headerInformation, expectedClass, allowChildClass);
      return AllSerializableStrategy.read(this, fileReader, actualClass);
   }

   public void readFieldsReflectively(final Object instance)
   {
      ReflectionSerializableStrategy.read(this, instance);
   }

   public ObjectReaderRegistry getObjectRegistry()
   {
      return registry;
   }
}
