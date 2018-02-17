package com.github.skySpiral7.java.staticSerialization;

import java.io.Closeable;
import java.io.File;
import java.util.Objects;

import com.github.skySpiral7.java.AsynchronousFileReader;
import com.github.skySpiral7.java.exception.NoMoreDataException;
import com.github.skySpiral7.java.staticSerialization.strategy.AllSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.InternalStreamReader;
import com.github.skySpiral7.java.staticSerialization.strategy.ReaderValidationStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ReflectionSerializableStrategy;
import com.github.skySpiral7.java.util.ClassUtil;

import static com.github.skySpiral7.java.util.ClassUtil.cast;

public class ObjectStreamReader implements Closeable
{
   private final ObjectReaderRegistry registry;
   private final InternalStreamReader internalStreamReader;

   public ObjectStreamReader(final File sourceFile)
   {
      registry = new ObjectReaderRegistry();
      internalStreamReader = new InternalStreamReader(sourceFile);
   }

   /**
    * @see AsynchronousFileReader#close()
    */
   @Override
   public void close(){internalStreamReader.close();}

   public boolean hasData(){return internalStreamReader.getFileReader().hasData();}

   public int remainingBytes(){return internalStreamReader.getFileReader().remainingBytes();}

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
    * <p>Security limitation: for multidimensional arrays only the topmost is matched exactly. Therefore if the
    * base component is not a final class (or primitive) then you must trust all possible children. In the case
    * of an Object array (not supported) this means trusting all classes which makes this method nearly pointless
    * (failing only if the root is different).</p>
    *
    * @see #readObject(Class)
    */
   public <T> T readObjectStrictly(final Class<T> expectedClass)
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
   public <T> T readObject(final Class<T> expectedClass)
   {
      return readObjectInternal(expectedClass, true);
   }

   /**
    * @param allowChildClass true will throw if the class found isn't the exact same. false allows casting.
    */
   private <T_Expected, T_Actual extends T_Expected> T_Actual readObjectInternal(final Class<T_Expected> expectedClass,
                                                                                 final boolean allowChildClass)
   {
      Objects.requireNonNull(expectedClass);
      return internalStreamReader.readObjectInternal(this, null, expectedClass, allowChildClass);
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
