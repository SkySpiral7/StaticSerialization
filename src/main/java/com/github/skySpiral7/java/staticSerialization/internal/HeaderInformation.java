package com.github.skySpiral7.java.staticSerialization.internal;

import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;

import java.util.Objects;

/**
 * An immutable bean to hold the information that the stream's header contains. It is returned by HeaderSerializableStrategy.
 *
 * @param <T_Value> The type whose name is className.
 * @see HeaderSerializableStrategy
 */
public final class HeaderInformation<T_Value>
   //TODO: confirm no raw types
{
   private final String className;
   private final T_Value value;
   private final int dimensionCount;
   private final boolean primitiveArray;

   /**
    * @return HeaderInformation to represent a header with a null value.
    */
   public static HeaderInformation<?> forNull()
   {
      return new HeaderInformation<>(null, null, 0, false);
   }

   /**
    * @param boxClassName must be the class name of the box not the primitive
    * @return HeaderInformation to represent an element within a primitive array (which has no header).
    */
   public static HeaderInformation<?> forPrimitiveArrayValue(final String boxClassName)
   {
      //TODO: isn't this only possible with 2d+? in which case rename forInheritedPrimitiveArray
      return new HeaderInformation<>(boxClassName, null, 0, false);
   }

   /**
    * @param dimensionCount the number of array dimensions (0 if not an array)
    * @return a HeaderInformation without a value (this is the norm)
    */
   public static HeaderInformation<?> forPossibleArray(final String baseComponentClassName, final int dimensionCount,
                                                       final boolean primitiveArray)
   {
      return new HeaderInformation<>(baseComponentClassName, null, dimensionCount, primitiveArray);
   }

   /**
    * @return HeaderInformation with the given value and 0 array dimensions (ie not an array).
    */
   public static <T_Value> HeaderInformation<T_Value> forValue(final String boxClassName, final T_Value value)
   {
      return new HeaderInformation<>(boxClassName, value, 0, false);
   }

   /**
    * For private use and testing only. Takes every value as-is.
    */
   HeaderInformation(final String className, final T_Value value, final int dimensionCount, final boolean primitiveArray)
   {
      this.className = className;
      this.value = value;
      this.dimensionCount = dimensionCount;
      this.primitiveArray = primitiveArray;
   }

   /**
    * @return the class name of the stream's object. Will never be a primitive class or void. Will be null if the stream's object was null.
    * Arrays return the base component class name. Primitive classes will be boxed (even for arrays).
    * @see #isPrimitiveArray()
    */
   public String getClassName(){return className;}

   /**
    * If {@link #getClassName()} is null then the header represents null. Else null means there is no value. There will be a value for true,
    * false, null, or an existing id.
    *
    * @return the value of the stream's object if any (most values aren't in the header)
    */
   public T_Value getValue(){return value;}

   /**
    * @return the number of array dimensions (0 if not an array)
    */
   public int getDimensionCount(){return dimensionCount;}

   /**
    * @return true if isArray AND base component is a primitive class
    */
   public boolean isPrimitiveArray()
   {
      return primitiveArray;
   }

   @Override
   public boolean equals(final Object other)
   {
      if (this == other) return true;
      if (other == null || getClass() != other.getClass()) return false;
      final HeaderInformation<?> that = (HeaderInformation<?>) other;
      return Objects.equals(className, that.className) && Objects.equals(value, that.value) && Objects.equals(dimensionCount,
         that.dimensionCount) && Objects.equals(primitiveArray, that.primitiveArray);
   }

   @Override
   public int hashCode()
   {
      return Objects.hash(className, value, dimensionCount, primitiveArray);
   }

   @Override
   public String toString()
   {
      if (null == className) return "null";
      if (Boolean.TRUE.equals(value)) return "true";
      if (Boolean.FALSE.equals(value)) return "false";
      if (0 == dimensionCount) return className;
      if (primitiveArray) return dimensionCount + "d array of primitive " + className;
      return dimensionCount + "d array of " + className;
   }
}
