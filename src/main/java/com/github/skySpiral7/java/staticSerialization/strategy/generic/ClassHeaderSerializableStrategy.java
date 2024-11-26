package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ReaderValidationStrategy;

public class ClassHeaderSerializableStrategy implements SerializableStrategy
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
   public Class<?> readHeader(final Class<?> inheritFromClass, final HeaderSerializableStrategy.PartialHeader partialHeader, final Class<?> expectedClass, final boolean allowChildClass)
   {
      //firstByte is part of a class name
      final String className = "" + ((char) partialHeader.firstByte()) + stringSerializableStrategy.read(null);
      final HeaderInformation<?> headerInformation = HeaderInformation.forPossibleArray(partialHeader.firstByte(), className, partialHeader.dimensionCount(),
         partialHeader.primitiveArray());
      return readerValidationStrategy.getClassFromHeader(headerInformation, expectedClass, allowChildClass);
   }

   @Override
   public boolean supportsData(final Class<?> actualClass)
   {
      return false;
   }

   @Override
   public void write(final Object rawData)
   {
      throw new IllegalStateException("Not implemented");
   }

   @Override
   public <T> T read(final Class<T> actualClass)
   {
      throw new IllegalStateException("Not implemented");
   }
}
