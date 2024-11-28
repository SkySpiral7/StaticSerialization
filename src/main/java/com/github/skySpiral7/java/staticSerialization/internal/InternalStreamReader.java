package com.github.skySpiral7.java.staticSerialization.internal;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.strategy.AllSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ReaderValidationStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ReflectionSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.StrategyInstances;
import com.github.skySpiral7.java.staticSerialization.stream.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Closeable;
import java.io.File;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class InternalStreamReader implements Closeable
{
   private static final Logger LOG = LogManager.getLogger();
   private final ObjectStreamReader streamReader;
   private final EasyReader reader;
   private final ObjectReaderRegistry registry;
   private final ClassUtil classUtil;
   private final AllSerializableStrategy allSerializableStrategy;
   private final ReaderValidationStrategy readerValidationStrategy;
   private final ReflectionSerializableStrategy reflectionSerializableStrategy;

   public InternalStreamReader(final ObjectStreamReader streamReader, final File sourceFile)
   {
      this(streamReader, new AsynchronousFileReader(sourceFile));
   }

   public InternalStreamReader(final ObjectStreamReader streamReader, final EasyReader reader)
   {
      this(streamReader, reader, new ObjectReaderRegistry(), new UtilInstances());
   }

   private InternalStreamReader(final ObjectStreamReader streamReader, final EasyReader reader, final ObjectReaderRegistry registry,
                                final UtilInstances utilInstances)
   {
      final StrategyInstances strategyInstances = new StrategyInstances(streamReader, this,
         reader, registry, utilInstances);
      this.streamReader = streamReader;
      this.reader = reader;
      this.registry = registry;
      this.classUtil = utilInstances.getClassUtil();
      allSerializableStrategy = strategyInstances.getAllSerializableStrategy();
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
   public <T_Expected, T_Actual extends T_Expected> T_Actual readObjectInternal(final Class<?> inheritFromClass,
                                                                                Class<T_Expected> expectedClass,
                                                                                final boolean allowChildClass)
   {
      //must check for void.class because ClassUtil.boxClass would throw something less helpful
      if (void.class.equals(expectedClass)) throw new IllegalArgumentException("There are no instances of void");
      if (expectedClass.isPrimitive()) expectedClass = cast(classUtil.boxClass(expectedClass));

      final HeaderInformation<?> headerInformation = allSerializableStrategy.readHeader(
         inheritFromClass, null, expectedClass, allowChildClass);
      LOG.debug("read header: {}", headerInformation);
      if (headerHasValue(headerInformation, expectedClass, allowChildClass))
      {
         return readHeaderValue(headerInformation, expectedClass, allowChildClass);
      }

      //TODO: move header reading into the strats
      final Class<T_Actual> actualClass = cast(readHeaderClass(headerInformation, expectedClass, allowChildClass));
      if (!classUtil.isPrimitiveOrBox(actualClass))
      {
         registry.reserveIdForLater();
      }
      final T_Actual returnValue = allSerializableStrategy.readData(actualClass);
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

   public boolean headerHasValue(final HeaderInformation<?> headerInformation,
                                 final Class<?> expectedClass,
                                 final boolean allowChildClass)
   {
      //TODO: throw new IllegalStateException("Expected: int, Actual: null, Consider using Integer")
      //if cast it will NPE is that better? what about allowing children?
      if (headerInformation.getClassName() == null) return true;
      if (headerInformation.getDimensionCount() == 0 && Boolean.class.getName().equals(headerInformation.getClassName()))
      {
         readerValidationStrategy.validateBoolean(expectedClass, allowChildClass);
         if (headerInformation.getValue() != null) return true;
         //will be null for primitive arrays or if the header explicitly contained Boolean for some reason
         //either way will be read below
      }
      else if (headerInformation.getValue() != null)
      {
         return true;
      }
      return false;
   }

   public <T> T readHeaderValue(final HeaderInformation<?> headerInformation,
                                final Class<?> expectedClass,
                                final boolean allowChildClass)
   {
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

      throw new IllegalStateException("header has no value");
   }

   public Class<?> readHeaderClass(final HeaderInformation<?> headerInformation,
                                   final Class<?> expectedClass,
                                   final boolean allowChildClass)
   {
      final Class<?> actualClass;
      if (headerInformation.getKnownClass() != null)
      {
         actualClass = cast(headerInformation.getKnownClass());
      }
      else
      {
         //TODO: split in half
         actualClass = readerValidationStrategy.getClassFromHeader(headerInformation,
            expectedClass, allowChildClass);
      }
      return actualClass;
   }

   public AllSerializableStrategy getAllSerializableStrategy()
   {
      return allSerializableStrategy;
   }
}
