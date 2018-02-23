package com.github.skySpiral7.java.staticSerialization.strategy;

import java.lang.reflect.Array;

import com.github.skySpiral7.java.staticSerialization.exception.DeserializationException;
import com.github.skySpiral7.java.staticSerialization.util.ArrayUtil;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public enum ReaderValidationStrategy
{
   ;  //no instances

   public static <T> void validateBoolean(final Class<T> expectedClass, final boolean allowChildClass)
   {
      if (!allowChildClass && !Boolean.class.equals(expectedClass))
         throw new IllegalStateException("Class doesn't match exactly. Expected: " + expectedClass.getName() + " Got: java.lang.Boolean");
      if (!expectedClass.isAssignableFrom(Boolean.class))
         //Not redundant because it needs to fail based on expectedClass after type erasure before getting to the client.
         //Same message as JVM.
         throw new ClassCastException(Boolean.class.getName() + " cannot be cast to " + expectedClass.getName());
   }

   public static <T_Expected, T_Actual extends T_Expected> Class<T_Actual> getClassFromHeader(final HeaderInformation actualHeader,
                                                                                              final Class<T_Expected> expectedClass,
                                                                                              final boolean allowChildClass)
   {
      final int expectedDimensions = ArrayUtil.countArrayDimensions(expectedClass);
      final Class<?> expectedBaseComponentType = ArrayUtil.getBaseComponentType(expectedClass);
      //TODO: dimension count must always match except for Object
      if (!allowChildClass)
      {
         if (expectedClass.isArray())
         {
            final HeaderInformation expectedHeader;
            if (expectedBaseComponentType.isPrimitive())
               expectedHeader = new HeaderInformation(ClassUtil.boxClass(expectedBaseComponentType).getName(), expectedDimensions, true);
            else expectedHeader = new HeaderInformation(expectedBaseComponentType.getName(), expectedDimensions, false);

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

      Class<?> actualClass;
      try
      {
         //No support for "int" etc. primitives can only reach here by someone else writing "int" in the stream.
         //Since Class.forName doesn't support primitives it likewise won't allow primitive void.
         actualClass = Class.forName(actualHeader.getClassName());
      }
      catch (final ClassNotFoundException classNotFoundException)
      {
         throw new DeserializationException(classNotFoundException);
      }
      if (0 != actualHeader.getDimensionCount())
      {
         if (actualHeader.isPrimitiveArray()) actualClass = ClassUtil.unboxClass(actualClass);
         //TODO: tests are likely thin
         if (!Object.class.equals(expectedClass) && !expectedBaseComponentType.isAssignableFrom(actualClass))
            //Not redundant because this is the only check for empty arrays
            //and checking here is better than waiting for failing to set an element in the array.
            //Same message as JVM.
            //TODO: consider: I could add code to allow primitive array to be cast into box. But what about widening etc?
            throw new ClassCastException(actualClass.getName() + " cannot be cast to " + expectedBaseComponentType.getName());
         final int[] arrayOfLengths = new int[actualHeader.getDimensionCount()];  //they are filled with 0 by default
         //It is easier to create an empty array then a string that would match the class name.
         return cast(Array.newInstance(actualClass, arrayOfLengths).getClass());
      }
      if (!expectedClass.isAssignableFrom(actualClass))
         //Not redundant because it needs to fail based on expectedClass after type erasure before getting to the client.
         //Same message as JVM.
         throw new ClassCastException(actualClass.getName() + " cannot be cast to " + expectedClass.getName());
      return cast(actualClass);
   }
}
