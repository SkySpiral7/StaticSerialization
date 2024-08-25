package com.github.skySpiral7.java.staticSerialization.exception;

import java.io.Serial;

/**
 * This exception is thrown when a class violates a contract that can't be enforced by an interface, such as expecting a no-arg constructor
 * or a static method. Use an interface when possible for things that it can enforce.
 */
public class InvalidClassException extends RuntimeException
{
   @Serial
   private static final long serialVersionUID = 0;

   /**
    * Constructs a {@code InvalidClassException} with the specified detail message.
    */
   public InvalidClassException(final String detailMessage){super(detailMessage);}

   /**
    * Constructs a {@code InvalidClassException} indicating the invalid class and why it's invalid.
    */
   public InvalidClassException(final String detailMessage, final Class<?> invalidClass)
   {
      super(detailMessage + " Class: " + invalidClass.getName());
   }
}
