package com.github.SkySpiral7.Java.exception;

import java.io.EOFException;
import java.util.NoSuchElementException;

/**
 * This exception is thrown when a client attempts to consume more data than remains.
 * This is usually used for streams but can be used more generally. Rather than being an unchecked alternative to EOFException,
 * it is more similar in nature to NoSuchElementException.
 * Instead of throwing this exception consider alternatives such as returning null, Optional, or a special value.
 *
 * @see NoSuchElementException
 * @see EOFException EOFException which is used for both end of file and end of stream
 */
public class NoMoreDataException extends RuntimeException
{
   private static final long serialVersionUID = 0;

   /**
    * Constructs a <code>NoMoreDataException</code> with no detail message in order to indicate that there are 0 bytes remaining.
    */
   public NoMoreDataException(){}

   /**
    * Constructs a <code>NoMoreDataException</code> with the specified detail message.
    */
   public NoMoreDataException(final String detailMessage)
   {
      super(detailMessage);
   }

   /**
    * Constructs a <code>NoMoreDataException</code> indicating that there aren't enough remaining bytes.
    */
   public NoMoreDataException(final long expectedByteCount, final long remainingByteCount)
   {
      super("expected " + expectedByteCount + " bytes, found " + remainingByteCount + " bytes");
   }
}
