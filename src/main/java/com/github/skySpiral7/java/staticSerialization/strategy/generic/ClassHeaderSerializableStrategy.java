package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.strategy.ReaderValidationStrategy;

public class ClassHeaderSerializableStrategy implements HeaderStrategy
{
   private final StringSerializableStrategy stringSerializableStrategy;
   private final ReaderValidationStrategy readerValidationStrategy;

   public ClassHeaderSerializableStrategy(final StringSerializableStrategy stringSerializableStrategy, final ReaderValidationStrategy readerValidationStrategy)
   {
      this.stringSerializableStrategy = stringSerializableStrategy;
      this.readerValidationStrategy = readerValidationStrategy;
   }

   /**
    * For writing.
    */
   public ClassHeaderSerializableStrategy(final StringSerializableStrategy stringSerializableStrategy)
   {
      this.stringSerializableStrategy = stringSerializableStrategy;
      this.readerValidationStrategy = null;
   }

   @Override
   public boolean supportsReadingHeader(final byte firstByte)
   {
      return (
         /*
         FQ class names can't start with . or a number.
         Class names can start with $
         I don't think packages can but $ is possible here with no package (default package).
         */
         ('a' <= firstByte && firstByte <= 'z')
            || ('A' <= firstByte && firstByte <= 'Z')
            || '$' == firstByte
      );
   }

   @Override
   public HeaderInformation<?> readHeader(final Class<?> inheritFromClass,
                                          final HeaderInformation.PartialHeader partialHeader,
                                          final Class<?> expectedClass,
                                          final boolean allowChildClass)
   {
      //firstByte is part of a class name
      final String className = "" + ((char) partialHeader.firstByte()) + stringSerializableStrategy.readData(null);
      final HeaderInformation<?> headerInformation = HeaderInformation.forPossibleArray(className, partialHeader.dimensionCount(),
         partialHeader.primitiveArray());
      readerValidationStrategy.getClassFromHeader(headerInformation, expectedClass, allowChildClass);
      return headerInformation;
   }

   @Override
   public boolean supportsWritingHeader(final Class<?> inheritFromClass, final Object data)
   {
      return true;
   }

   @Override
   public boolean writeHeader(final Class<?> inheritFromClass, final Object data)
   {
      stringSerializableStrategy.writeData(data.getClass().getName());
      return false;
   }
}
