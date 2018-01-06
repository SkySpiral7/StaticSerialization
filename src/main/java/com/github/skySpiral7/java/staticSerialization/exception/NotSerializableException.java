package com.github.skySpiral7.java.staticSerialization.exception;

/**
 * This exception is thrown when attempting to serialize or deserialize an object that isn't Serializable.
 * This is an unchecked version of {@link java.io.NotSerializableException} and can be used for any type of Serialization.
 */
public class NotSerializableException extends RuntimeException
{
   private static final long serialVersionUID = 0;

   /**
    * Constructs a <code>NotSerializableException</code> with the specified detail message.
    */
   public NotSerializableException(final String detailMessage)
   {
      super(detailMessage);
   }

   /**
    * Constructs a <code>NotSerializableException</code> indicating the illegal class.
    */
   public NotSerializableException(final Class<?> nonSerializableClass)
   {
      super(nonSerializableClass.getName());
   }
}
