package com.github.SkySpiral7.Java.pojo;

import java.util.Objects;

/**
 * <p>An immutable bean used to represent the results of integer division.
 * An integer divided by an integer resulting in an integer is not always possible but sometimes required.
 * Therefore observe the definition for integer division and for the remainder.</p>
 *
 * <code>a / n is defined as a = (n * wholeResult) &plusmn; remainder</code><br />
 * <code>remainder = |a| - |(n * wholeResult)|</code><br /><br />
 *
 * <ul>
 * <li>The wholeResult may be any integer but 0 &lt;= remainder &lt; n (it isn't possible to validate this).</li>
 * <li>If a = 0 then remainder = wholeResult = 0.</li>
 * <li>If n = 0 then remainder = a and wholeResult = 0 (although any number would be correct)</li>
 * <li>If a &lt; n then remainder = a and wholeResult = 0 (notice that all 3 of these look the same).</li>
 * </ul>
 * <br /><br />
 *
 * @param <T> any child class of Number. Although only integers make sense.
 */
public final class IntegerQuotient<T extends Number>
{
   protected T wholeResult;
   protected T remainder;

   /**
    * Note that this constructor is the only way to create this immutable object.
    *
    * @throws NullPointerException if either parameter is null.
    * @see IntegerQuotient
    */
   public IntegerQuotient(T wholeResult, T remainder)
   {
      Objects.requireNonNull(wholeResult);
      Objects.requireNonNull(remainder);
      this.wholeResult = wholeResult;
      this.remainder = remainder;
   }

   /**
    * @return the integer that resulted from the division. It may be zero but not null.
    */
   public T getWholeResult(){return wholeResult;}

   /**
    * @return the integer that remained after the division. It may be zero but not null or negative.
    */
   public T getRemainder(){return remainder;}

   @Override
   public String toString()
   {
      return "whole=" + wholeResult + "; remainder=" + remainder;
   }

}
