package com.github.SkySpiral7.Java.exception;

/**
 * This exception is thrown when a class violates a contract that can't be enforced by an interface, such as expecting a no-arg constructor or a static method.
 * Use an interface when possible for things that it can enforce.
 */
public class InvalidClassException extends RuntimeException
{
   private static final long serialVersionUID = 0;

   /**
    * Constructs a <code>InvalidClassException</code> with the specified detail message.
    */
   public InvalidClassException(final String detailMessage) {super(detailMessage);}

   /**
    * Constructs a <code>InvalidClassException</code> indicating the invalid class and why it's invalid.
    */
   public InvalidClassException(final String detailMessage, final Class<?> invalidClass)
   {
      super(detailMessage + " Class: " + invalidClass.getName());
   }
}
