package com.github.skySpiral7.java.staticSerialization.internal;

import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;

import java.nio.charset.StandardCharsets;
import java.util.Objects;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

/**
 * An immutable bean to hold the information that the stream's header contains. It is returned by HeaderSerializableStrategy.
 *
 * @param <T_Value> The type whose name is className.
 * @see HeaderSerializableStrategy
 */
public final class HeaderInformation<T_Value>
   //TODO: confirm no raw types
{
   private final byte firstByte;
   private final String className;
   private final Class<T_Value> knownClass;
   private final T_Value value;
   private final int dimensionCount;
   private final boolean primitiveArray;

   /**
    * @return HeaderInformation to represent a header with a null value.
    */
   public static HeaderInformation<?> forNull(final byte firstByte)
   {
      return new HeaderInformation<>(firstByte, null, Object.class, null, 0, false);
   }

   /**
    * @return HeaderInformation to represent an element within a primitive array (which has no header).
    */
   public static <T_Value> HeaderInformation<T_Value> forPrimitiveArrayValue(final Class<T_Value> boxedClass)
   {
      //TODO: isn't this only possible with 2d+? in which case rename forInheritedPrimitiveArray
      //primitiveArray=false because this header info is for a primitive value not an array
      String boxClassName = boxedClass.getName();
      return new HeaderInformation<>(boxClassName.getBytes(StandardCharsets.UTF_8)[0], boxClassName, boxedClass, null, 0, false);
   }

   /**
    * @param dimensionCount the number of array dimensions (0 if not an array)
    * @return a HeaderInformation without a value (this is the norm)
    */
   public static <T_Value> HeaderInformation<T_Value> forPossibleArray(final byte firstByte, final Class<T_Value> baseComponentClass, final int dimensionCount,
                                                                       final boolean primitiveArray)
   {
      return new HeaderInformation<>(firstByte, baseComponentClass.getName(), null, null, dimensionCount, primitiveArray);
   }

   /**
    * @param dimensionCount the number of array dimensions (0 if not an array)
    * @return a HeaderInformation without a value (this is the norm)
    */
   public static HeaderInformation<?> forPossibleArray(final byte firstByte, final String baseComponentClassName, final int dimensionCount,
                                                       final boolean primitiveArray)
   {
      return new HeaderInformation<>(firstByte, baseComponentClassName, null, null, dimensionCount, primitiveArray);
   }

   /**
    * @return HeaderInformation with the given value and 0 array dimensions (ie not an array).
    */
   public static <T_Value> HeaderInformation<T_Value> forValue(final byte firstByte, final String className, final T_Value value)
   {
      return new HeaderInformation<>(firstByte, className, cast(value.getClass()), value, 0, false);
   }

   /**
    * For private use and testing only. Takes every value as-is.
    */
   public HeaderInformation(final byte firstByte, final String className, final Class<T_Value> knownClass, final T_Value value, final int dimensionCount,
                            final boolean primitiveArray)
   {
      this.firstByte = firstByte;
      this.className = className;
      this.knownClass = knownClass;
      this.value = value;
      this.dimensionCount = dimensionCount;
      this.primitiveArray = primitiveArray;
   }

   /**
    * @return the first byte read from stream. Will be either a compressed header or first byte of class name.
    */
   public byte getFirstByte()
   {
      //TODO: remove getFirstByte
      return firstByte;
   }

   /**
    * @return the class name of the stream's object. Will never be a primitive class or void. Will be null if the stream's object was null.
    * Arrays return the base component class name. Primitive classes will be boxed (even for arrays).
    * @see #isPrimitiveArray()
    */
   public String getClassName(){return className;}

   /**
    * @return only non-null if an already loaded class. If non-null will match {@link #className}
    */
   public Class<T_Value> getKnownClass()
   {
      return knownClass;
   }

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
