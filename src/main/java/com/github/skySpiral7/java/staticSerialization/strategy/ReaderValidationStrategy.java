package com.github.skySpiral7.java.staticSerialization.strategy;

import java.lang.reflect.Array;

import com.github.skySpiral7.java.staticSerialization.exception.DeserializationException;
import com.github.skySpiral7.java.util.ArrayUtil;

import static com.github.skySpiral7.java.util.ClassUtil.cast;

public enum ReaderValidationStrategy
{
   ;  //no instances

   public static <T> void validateBoolean(final Class<T> expectedClass, final boolean allowChildClass)
   {
      if (!allowChildClass && !Boolean.class.equals(expectedClass))
         throw new IllegalStateException("Class doesn't match exactly. Expected: " + expectedClass.getName() + " Got: java.lang.Boolean");
      //TODO: I think this is redundant?
      if (!expectedClass.isAssignableFrom(Boolean.class))
         //Same message as JVM.
         throw new ClassCastException(Boolean.class.getName() + " cannot be cast to " + expectedClass.getName());
   }

   public static <T_Expected, T_Actual extends T_Expected> Class<T_Actual> getClassFromHeader(final HeaderInformation actualHeader,
                                                                                              final Class<T_Expected> expectedClass,
                                                                                              final boolean allowChildClass)
   {
      final int expectedDimensions = ArrayUtil.countArrayDimensions(expectedClass);
      final Class<?> expectedBaseComponentType = ArrayUtil.getBaseComponentType(expectedClass);
      if (!allowChildClass)
      {
         if (expectedClass.isArray())
         {
            final HeaderInformation expectedHeader = new HeaderInformation(expectedBaseComponentType.getName(), expectedDimensions);
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

      final Class<?> actualClass;
      try
      {
         actualClass = Class.forName(actualHeader.getClassName());
      }
      catch (final ClassNotFoundException classNotFoundException)
      {
         throw new DeserializationException(classNotFoundException);
      }
      if (expectedClass.isArray())
      {
         if (!expectedBaseComponentType.isAssignableFrom(actualClass))
            //Not redundant because this is the only check for empty arrays
            //and checking here is better than waiting for failing to set an element in the array.
            //Same message as JVM.
            throw new ClassCastException(actualClass.getName() + " cannot be cast to " + expectedBaseComponentType.getName());
         final int[] arrayOfLengths = new int[expectedDimensions];  //they are filled with 0 by default
         //It is easier to create an empty array then a string that would match the class name.
         return cast(Array.newInstance(expectedBaseComponentType, arrayOfLengths).getClass());
      }
      if (!expectedClass.isAssignableFrom(actualClass))
         //Not redundant because it needs to fail based on expectedClass after type erasure before getting to the client.
         //Same message as JVM.
         //TODO: make sure that this throws when actualClass is primitive void
         throw new ClassCastException(actualClass.getName() + " cannot be cast to " + expectedClass.getName());
      return cast(actualClass);
   }
}
