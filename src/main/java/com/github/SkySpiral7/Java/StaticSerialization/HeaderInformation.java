package com.github.SkySpiral7.Java.StaticSerialization;

import java.util.Objects;

/**
 * A bean to hold the information that the stream's header contains. It is returned by ObjectHeaderReader.
 *
 * @see ObjectHeaderReader
 */
public final class HeaderInformation
{
   private final String className;
   private final Boolean value;

   /**
    * Constructed with a null value (which is the norm).
    */
   public HeaderInformation(final String className)
   {
      this.className = className;
      this.value = null;
   }

   public HeaderInformation(final String className, final Boolean value)
   {
      this.className = className;
      this.value = value;
   }

   /**
    * @return the class name of the stream's object. Will never be a primitive class or void.
    * Will be null if the stream's object was null.
    */
   public String getClassName()
   {
      return className;
   }

   /**
    * The only possible values that are in the header are true, false, (and null which isn't a exactly a value).
    *
    * @return the value of the stream's object if any (most likely null)
    */
   public Boolean getValue()
   {
      return value;
   }

   @Override
   public boolean equals(final Object other)
   {
      if (this == other) return true;
      if (other == null || getClass() != other.getClass()) return false;
      final HeaderInformation that = (HeaderInformation) other;
      return Objects.equals(className, that.className) && Objects.equals(value, that.value);
   }

   @Override
   public int hashCode()
   {
      return Objects.hash(className, value);
   }

   @Override
   public String toString()
   {
      final StringBuilder sb = new StringBuilder("HeaderInformation{");
      sb.append("className='").append(className).append('\'');
      sb.append(", value=").append(value);
      sb.append('}');
      return sb.toString();
   }
}
