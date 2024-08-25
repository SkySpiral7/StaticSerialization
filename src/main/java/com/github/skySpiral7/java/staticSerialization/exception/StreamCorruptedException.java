package com.github.skySpiral7.java.staticSerialization.exception;

import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;

import java.io.Serial;

/**
 * This exception is thrown when attempting to read invalid data in the stream's header. Do not throw this if an object is deserialized in a
 * way that violates invariants. This exception is only for invalid meta information not invalid variable values and thus is likely only
 * going to be thrown by ObjectStreamReader.
 * @see DeserializationException
 */
//TODO: StreamCorruptedException is also used for bad enum or missing data but maybe should have new exception for those
public class StreamCorruptedException extends RuntimeException
{
   @Serial
   private static final long serialVersionUID = 0;

   /**
    * Constructs a {@code StreamCorruptedException} with the specified detail message.
    */
   public StreamCorruptedException(final String detailMessage)
   {
      super(detailMessage);
   }

   /**
    * Constructs a {@code StreamCorruptedException} with the specified cause.
    */
   public StreamCorruptedException(final Throwable cause)
   {
      super(cause);
   }

   /**
    * Constructs a {@code StreamCorruptedException} with the detail message and cause.
    */
   public StreamCorruptedException(final String detailMessage, final Throwable cause)
   {
      super(detailMessage, cause);
   }

   /**
    * This method attempts to read data. That data will be returned if valid or StreamCorruptedException will be thrown
    * if there aren't enough bytes remaining.
    *
    * @param reader         the reader to read the data from
    * @param requiredBytes  the number of bytes that must be read when valid
    * @param corruptMessage the message to use if there aren't enough bytes
    * @return the data read with a length of {@code requiredBytes}
    * @throws StreamCorruptedException if there aren't enough bytes
    */
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

   /**
    * This method attempts to read data. That data will be returned if valid or StreamCorruptedException will be thrown
    * if the data isn't terminated by the expected byte.
    *
    * @param reader         the reader to read the data from
    * @param finalByte      the terminating byte that must exist when valid
    * @param corruptMessage the message to use if the byte isn't found
    * @return the data read which will end with {@code finalByte}
    * @throws StreamCorruptedException if the byte isn't found
    */
   public static byte[] throwIfNotByteTerminated(final EasyReader reader, final byte finalByte, final String corruptMessage)
   {
      final byte[] result = reader.readBytesUntil(finalByte);
      if (result.length == 0 || result[result.length - 1] != finalByte)
         throw new StreamCorruptedException(corruptMessage);
      return result;
   }
}
