package com.github.skySpiral7.java.staticSerialization.exception;

import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;

/**
 * This exception is thrown when attempting to read invalid data in the stream's header. Do not throw this if an object is deserialized in a
 * way that violates invariants. This exception is only for invalid meta information not invalid variable values and thus is likely only
 * going to be thrown by ObjectStreamReader.
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

   /**
    * Constructs a <code>StreamCorruptedException</code> with the detail message and cause.
    */
   public StreamCorruptedException(final String detailMessage, final Throwable cause)
   {
      super(detailMessage, cause);
   }

   public static byte[] throwIfNotEnoughData(final EasyReader reader, final int requiredBytes, final String corruptMessage)
   {
      final byte[] result = reader.readBytes(requiredBytes);
      if (requiredBytes > result.length)
         throw new StreamCorruptedException(
            corruptMessage,
            NoMoreDataException.notEnoughBytes(requiredBytes, result.length)
         );
      return result;
   }
}
