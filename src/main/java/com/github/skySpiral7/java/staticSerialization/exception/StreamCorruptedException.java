package com.github.skySpiral7.java.staticSerialization.exception;

/**
 * This exception is thrown when attempting to read invalid data in the stream's header.
 * Do not throw this if an object is deserialized in a way that violates invariants.
 * This exception is only for invalid meta information not invalid variable values and
 * thus is likely only going to be thrown by ObjectStreamReader.
 */
public class StreamCorruptedException extends RuntimeException
{
   private static final long serialVersionUID = 0;

   /**
    * Constructs a <code>StreamCorruptedException</code> with the specified detail message.
    */
   public StreamCorruptedException(final String detailMessage)
   {
      super(detailMessage);
   }

   /**
    * Constructs a <code>StreamCorruptedException</code> with the specified cause.
    */
   public StreamCorruptedException(final Throwable cause)
   {
      super(cause);
   }
}
