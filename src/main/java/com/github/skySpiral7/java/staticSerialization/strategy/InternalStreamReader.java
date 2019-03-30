package com.github.skySpiral7.java.staticSerialization.strategy;

import java.io.Closeable;
import java.io.File;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.exception.NoMoreDataException;
import com.github.skySpiral7.java.staticSerialization.fileWrapper.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class InternalStreamReader implements Closeable
{
   private final AsynchronousFileReader fileReader;

   public InternalStreamReader(final File sourceFile)
   {
      fileReader = new AsynchronousFileReader(sourceFile);
   }

   /**
    * @see AsynchronousFileReader#close()
    */
   @Override
   public void close(){fileReader.close();}

   /**
    * @param allowChildClass true will throw if the class found isn't the exact same. false allows casting.
    */
   public <T_Expected, T_Actual extends T_Expected> T_Actual readObjectInternal(final ObjectStreamReader streamReader,
                                                                                final Class<?> inheritFromClass,
                                                                                Class<T_Expected> expectedClass,
                                                                                final boolean allowChildClass)
   {
      if (!fileReader.hasData()) throw new NoMoreDataException();

      //must check for void.class because ClassUtil.boxClass would throw something less helpful
      if (void.class.equals(expectedClass)) throw new IllegalArgumentException("There are no instances of void");
      if (expectedClass.isPrimitive()) expectedClass = cast(ClassUtil.boxClass(expectedClass));

      final HeaderInformation<?> headerInformation = HeaderSerializableStrategy.readHeader(fileReader, inheritFromClass,
            streamReader.getObjectRegistry());
      //TODO: throw new StreamCorruptedException("Expected: int, Actual: null, Consider using Integer")
      if (headerInformation.getClassName() == null) return null;  //can be cast to anything safely
      if (headerInformation.getDimensionCount() == 0 && Boolean.class.getName().equals(headerInformation.getClassName()))
      {
         ReaderValidationStrategy.validateBoolean(expectedClass, allowChildClass);
         if (headerInformation.getValue() != null) return cast(headerInformation.getValue());  //either true or false
         //will be null for primitive arrays or if the header explicitly contained Boolean for some reason
         //either way will be read below
         //TODO: add tests for header of "java.lang.Boolean"
      }
      else if (headerInformation.getValue() != null)
      {
         //TODO: test and validate
         //ReaderValidationStrategy.validateId(expectedClass, headerInformation.getValue(), allowChildClass);
         return cast(headerInformation.getValue());
      }

      final Class<T_Actual> actualClass = ReaderValidationStrategy.getClassFromHeader(headerInformation, expectedClass, allowChildClass);
      if (!actualClass.isPrimitive() && !ClassUtil.isBoxedPrimitive(actualClass))
      {
         streamReader.getObjectRegistry().reserveIdForLater();
      }
      final T_Actual returnValue = AllSerializableStrategy.read(streamReader, this, fileReader, actualClass);
      //null, boolean, and id don't reach here
      //TODO: test
      //TODO: make util for should register since long should
      if (!returnValue.getClass().isPrimitive() && !ClassUtil.isBoxedPrimitive(returnValue.getClass()))
         streamReader.getObjectRegistry().registerObject(returnValue);
      return returnValue;
   }

   public AsynchronousFileReader getFileReader()
   {
      return fileReader;
   }
}
