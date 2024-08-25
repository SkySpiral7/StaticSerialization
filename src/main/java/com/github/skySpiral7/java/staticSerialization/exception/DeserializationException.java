package com.github.skySpiral7.java.staticSerialization.exception;

import java.io.Serial;

/**
 * This exception is thrown when deserialization fails and there's no more specific exception to be thrown. This should not be thrown if an
 * object is deserialized in a way that violates invariants. This exception is only for exceptions that occur during the deserialization
 * process itself such as invalid meta information not invalid variable values and thus is likely only going to be thrown by
 * ObjectStreamReader.
 */
public class DeserializationException extends RuntimeException
{
   @Serial
   private static final long serialVersionUID = 0;

   /**
    * Constructs a <code>DeserializationException</code> with the specified detail message.
    */
   public DeserializationException(final String detailMessage)
   {
      super(detailMessage);
   }

   /**
    * Constructs a <code>DeserializationException</code> with the specified cause.
    */
   public DeserializationException(final Throwable cause)
   {
      super(cause);
   }
}
