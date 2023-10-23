package com.github.skySpiral7.java.staticSerialization.internal;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.strategy.AllSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ReaderValidationStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ReflectionSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.StrategyInstances;
import com.github.skySpiral7.java.staticSerialization.stream.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;

import java.io.Closeable;
import java.io.File;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class InternalStreamReader implements Closeable
{
   private final EasyReader reader;
   private final ObjectReaderRegistry registry;
   private final ClassUtil classUtil;
   private final AllSerializableStrategy allSerializableStrategy;
   private final HeaderSerializableStrategy headerSerializableStrategy;
   private final ReaderValidationStrategy readerValidationStrategy;
   private final ReflectionSerializableStrategy reflectionSerializableStrategy;

   public InternalStreamReader(final File sourceFile)
   {
      this(new AsynchronousFileReader(sourceFile));
   }

   public InternalStreamReader(final EasyReader reader)
   {
      this(reader, new ObjectReaderRegistry(), new UtilInstances());
   }

   private InternalStreamReader(final EasyReader reader, final ObjectReaderRegistry registry,
                               final UtilInstances utilInstances)
   {
      this(reader, registry, utilInstances.getClassUtil(), new StrategyInstances(reader, registry, utilInstances));
   }

   public InternalStreamReader(final EasyReader reader, final ObjectReaderRegistry registry,
                               final ClassUtil classUtil, final StrategyInstances strategyInstances)
   {
      this.reader = reader;
      this.registry = registry;
      this.classUtil = classUtil;
      allSerializableStrategy = strategyInstances.getAllSerializableStrategy();
      headerSerializableStrategy = strategyInstances.getHeaderSerializableStrategy();
      readerValidationStrategy = strategyInstances.getReaderValidationStrategy();
      reflectionSerializableStrategy = strategyInstances.getReflectionSerializableStrategy();
   }

   /**
    * @see AsynchronousFileReader#close()
    */
   @Override
   public void close(){reader.close();}

   /**
    * @param allowChildClass true will throw if the class found isn't the exact same. false allows casting.
    */
   public <T_Expected, T_Actual extends T_Expected> T_Actual readObjectInternal(final ObjectStreamReader streamReader,
                                                                                final Class<?> inheritFromClass,
                                                                                Class<T_Expected> expectedClass,
                                                                                final boolean allowChildClass)
   {
      //must check for void.class because ClassUtil.boxClass would throw something less helpful
      if (void.class.equals(expectedClass)) throw new IllegalArgumentException("There are no instances of void");
      if (expectedClass.isPrimitive()) expectedClass = cast(classUtil.boxClass(expectedClass));

      final HeaderInformation<?> headerInformation = headerSerializableStrategy.readHeader(inheritFromClass);
      //TODO: throw new IllegalStateException("Expected: int, Actual: null, Consider using Integer")
      //if cast it will NPE is that better? what about allowing children?
      if (headerInformation.getClassName() == null) return null;  //can be cast to anything safely
      if (headerInformation.getDimensionCount() == 0 && Boolean.class.getName().equals(headerInformation.getClassName()))
      {
         readerValidationStrategy.validateBoolean(expectedClass, allowChildClass);
         if (headerInformation.getValue() != null) return cast(headerInformation.getValue());  //either true or false
         //will be null for primitive arrays or if the header explicitly contained Boolean for some reason
         //either way will be read below
      }
      else if (headerInformation.getValue() != null)
      {
         //TODO: add validation that the id matches the expected class
         //ReaderValidationStrategy.validateIdClass(expectedClass, headerInformation.getValue(), allowChildClass);
         return cast(headerInformation.getValue());
      }

      final Class<T_Actual> actualClass = readerValidationStrategy.getClassFromHeader(headerInformation,
         expectedClass, allowChildClass);
      if (!classUtil.isPrimitiveOrBox(actualClass))
      {
         registry.reserveIdForLater();
      }
      final T_Actual returnValue = allSerializableStrategy.read(streamReader, this, actualClass);
      //null, boolean, and id don't reach here
      if (null == returnValue) return null;  //only possible for null Boolean or Java Serial. TODO: can array?
      //TODO: make util for should register since long should
      if (!classUtil.isPrimitiveOrBox(returnValue.getClass()) && !streamReader.isRegistered(returnValue))
         streamReader.registerObject(returnValue);
      return returnValue;
   }

   public ObjectReaderRegistry getRegistry()
   {
      return registry;
   }

   public ReflectionSerializableStrategy getReflectionSerializableStrategy()
   {
      return reflectionSerializableStrategy;
   }
}
