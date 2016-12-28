package com.github.SkySpiral7.Java.numbers;

/**
 * Same as the JRE version except unchecked.
 *
 * @see java.lang.NumberFormatException
 */
public class NumberFormatException extends RuntimeException
{
   private static final long serialVersionUID = 1L;

   /**
    * Constructs a <code>NumberFormatException</code> with the
    * specified detail message.
    */
   public NumberFormatException(final String detailMessage)
   {
      super(detailMessage);
   }

   /**
    * Factory method for making a <code>NumberFormatException</code>
    * given the specified input which caused the error.
    *
    * @param invalidString
    *       the input causing the error
    */
   public static NumberFormatException forInputString(final String invalidString)
   {
      return new NumberFormatException("input string: \"" + invalidString + "\"");
   }

   /**
    * Factory method for making a <code>NumberFormatException</code>
    * given the specified input which caused the error.
    *
    * @param invalidString
    *       the input causing the error
    */
   public static NumberFormatException forInputRadix(final String invalidString, final int radix)
   {
      return new NumberFormatException("radix: " + radix + " input string: \"" + invalidString + "\"");
   }
}
