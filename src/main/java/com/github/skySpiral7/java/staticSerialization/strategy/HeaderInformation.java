package com.github.skySpiral7.java.staticSerialization.strategy;

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
    * Constructed to represent a header with a null value.
    */
   public HeaderInformation()
   {
      this.className = null;
      this.value = null;
      dimensionCount = 0;
      primitiveArray = false;
   }

   /**
    * Constructed to represent an element within a primitive array (which has no header).
    *
    * @param boxClassName must be the class name of the box not the primitive
    */
   public HeaderInformation(final String boxClassName)
   {
      this.className = boxClassName;
      value = null;
      this.dimensionCount = 0;
      this.primitiveArray = false;
   }

   /**
    * Constructed with a null value (which is the norm).
    */
   public HeaderInformation(final String baseComponentClassName, final int dimensionCount, final boolean primitiveArray)
   {
      this.className = baseComponentClassName;
      value = null;
      this.dimensionCount = dimensionCount;
      this.primitiveArray = primitiveArray;
   }

   //TODO: make static factories instead of knowing which constructor

   /**
    * Constructed with the given value and 0 array dimensions (ie not an array).
    */
   public HeaderInformation(final String boxClassName, final T_Value value)
   {
      this.className = boxClassName;
      this.value = value;
      dimensionCount = 0;
      primitiveArray = false;
   }

   /**
    * For testing only. Takes every value as-is.
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
    * @return the value of the stream's object if any (most doesn't have a value)
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
