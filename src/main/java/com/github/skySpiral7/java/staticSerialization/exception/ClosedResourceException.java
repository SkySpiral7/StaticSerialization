package com.github.skySpiral7.java.staticSerialization.exception;

import java.io.Serial;

/**
 * This exception is thrown when attempting to use an AutoCloseable resource that has already been closed. Instead of throwing this
 * exception consider alternatives such as doing nothing, returning null, returning Optional, or returning a special value.
 */
public class ClosedResourceException extends RuntimeException
{
   @Serial
   private static final long serialVersionUID = 0;

   /**
    * Constructs a <code>ClosedResourceException</code> with no detail message.
    */
   public ClosedResourceException(){}

   /**
    * Constructs a <code>ClosedResourceException</code> with the specified detail message.
    */
   public ClosedResourceException(final String detailMessage)
   {
      super(detailMessage);
   }
}
