package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ReaderValidationStrategy;

public class ClassHeaderSerializableStrategy implements HeaderStrategy
{
   private final StringSerializableStrategy stringSerializableStrategy;
   private final ReaderValidationStrategy readerValidationStrategy;

   /**For reading only*/
   public ClassHeaderSerializableStrategy(final StringSerializableStrategy stringSerializableStrategy, final ReaderValidationStrategy readerValidationStrategy)
   {
      this.stringSerializableStrategy = stringSerializableStrategy;
      this.readerValidationStrategy = readerValidationStrategy;
   }

   @Override
   public boolean supportsHeader(final byte firstByte)
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
                                          final HeaderSerializableStrategy.PartialHeader partialHeader,
                                          final Class<?> expectedClass,
                                          final boolean allowChildClass)
   {
      //firstByte is part of a class name
      final String className = "" + ((char) partialHeader.firstByte()) + stringSerializableStrategy.readData(null);
      final HeaderInformation<?> headerInformation = HeaderInformation.forPossibleArray(partialHeader.firstByte(), className, partialHeader.dimensionCount(),
         partialHeader.primitiveArray());
      readerValidationStrategy.getClassFromHeader(headerInformation, expectedClass, allowChildClass);
      return headerInformation;
   }
}
