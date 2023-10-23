package com.github.skySpiral7.java.staticSerialization.internal;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.strategy.AllSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.StrategyInstances;
import com.github.skySpiral7.java.staticSerialization.stream.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;

import java.io.Closeable;
import java.io.File;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class InternalStreamReader implements Closeable
{
   private final EasyReader reader;
   private final ObjectReaderRegistry registry;
   private final UtilInstances utilInstances;
   private final StrategyInstances strategyInstances;

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
      this(reader, registry, utilInstances, new StrategyInstances(reader, registry, utilInstances));
   }

   public InternalStreamReader(final EasyReader reader, final ObjectReaderRegistry registry,
                               final UtilInstances utilInstances, final StrategyInstances strategyInstances)
   {
      this.reader = reader;
      this.registry = registry;
      this.utilInstances = utilInstances;
      this.strategyInstances = strategyInstances;
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
      if (expectedClass.isPrimitive()) expectedClass = cast(utilInstances.getClassUtil().boxClass(expectedClass));

      final HeaderInformation<?> headerInformation = strategyInstances.getHeaderSerializableStrategy().readHeader(this,
         inheritFromClass);
      //TODO: throw new IllegalStateException("Expected: int, Actual: null, Consider using Integer")
      //if cast it will NPE is that better? what about allowing children?
      if (headerInformation.getClassName() == null) return null;  //can be cast to anything safely
      if (headerInformation.getDimensionCount() == 0 && Boolean.class.getName().equals(headerInformation.getClassName()))
      {
         strategyInstances.getReaderValidationStrategy().validateBoolean(expectedClass, allowChildClass);
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

      final Class<T_Actual> actualClass = strategyInstances.getReaderValidationStrategy().getClassFromHeader(headerInformation,
         expectedClass, allowChildClass);
      if (!utilInstances.getClassUtil().isPrimitiveOrBox(actualClass))
      {
         registry.reserveIdForLater();
      }
      final T_Actual returnValue = strategyInstances.getAllSerializableStrategy().read(streamReader, this, actualClass);
      //null, boolean, and id don't reach here
      if (null == returnValue) return null;  //only possible for null Boolean or Java Serial. TODO: can array?
      //TODO: make util for should register since long should
      if (!utilInstances.getClassUtil().isPrimitiveOrBox(returnValue.getClass()) && !streamReader.isRegistered(returnValue))
         streamReader.registerObject(returnValue);
      return returnValue;
   }

   public EasyReader getReader()
   {
      return reader;
   }

   public ObjectReaderRegistry getRegistry()
   {
      return registry;
   }

   public UtilInstances getUtilInstances()
   {
      return utilInstances;
   }

   public StrategyInstances getStrategyInstances()
   {
      return strategyInstances;
   }
}
