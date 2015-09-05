package com.github.SkySpiral7.Java.util;

import java.util.Arrays;
import java.util.Objects;

import com.github.SkySpiral7.Java.numbers.NumericOverflowException;
import com.github.SkySpiral7.Java.numbers.UncheckedNumberFormatException;

public final class RadixUtil {
   private RadixUtil(){}

   /**
    * All possible chars for representing a number as a String up to base 62.
    * private because arrays are mutable.
    */
   private final static char[] base62Digits = {
       '0' , '1' , '2' , '3' , '4' , '5' ,
       '6' , '7' , '8' , '9' , 'a' , 'b' ,
       'c' , 'd' , 'e' , 'f' , 'g' , 'h' ,
       'i' , 'j' , 'k' , 'l' , 'm' , 'n' ,
       'o' , 'p' , 'q' , 'r' , 's' , 't' ,
       'u' , 'v' , 'w' , 'x' , 'y' , 'z',
       'A' , 'B' ,
       'C' , 'D' , 'E' , 'F' , 'G' , 'H' ,
       'I' , 'J' , 'K' , 'L' , 'M' , 'N' ,
       'O' , 'P' , 'Q' , 'R' , 'S' , 'T' ,
       'U' , 'V' , 'W' , 'X' , 'Y' , 'Z'
   };

   /**
    * <p>Returns a String that represents value in the given number base (radix).
    * The minimum radix is 1 with a maximum of 62. For radix &gt; 36 upper case letters are used.
    * In such cases the String returned is case sensitive. The digits used are in this order:</p>
    *
    * <blockquote>
    *   {@code 0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ}
    * </blockquote>
    *
    * <p>This order is used to be compatible with Long.toString.</p>
    * <p><b>Special case</b>: Base 1 uses the character '1' instead of '0'.</p>
    *
    * @param value the number that is to be represented
    * @param radix the number base
    * @return a String that represents value in the given number base (radix).
    * @throws IllegalArgumentException for unsupported radix or if the base 1 number will not fit into a String
    * @see Long#toString(long, int)
    */
   public static String toString(long value, final int radix)
   {
   	enforceStandardRadix(radix);
		if(1 == value) return "1";  //true for every radix

		final boolean isNegative = (value < 0);
		//delegate for powers of 2 because Long uses a different (ie optimized) formula
		//but only if value is positive because these Long methods are unsigned
		if (!isNegative)
		{
			if(2 == radix) return Long.toBinaryString(value);
	   	if(8 == radix) return Long.toOctalString(value);
	   	if(16 == radix) return Long.toHexString(value);
		}

   	if(1 == radix) return toStringBase1(value, isNegative);
		if(0 == value) return "0";  //true for every radix except 1

   	//delegate to Long.toString(long, int) because it could be better optimized
   	if(radix <= Character.MAX_RADIX) return Long.toString(value, radix);
   	//Character.MAX_RADIX is max JRE supported radix not max possible

		return toStringStandardBase(value, radix, isNegative);
   }

	/**
	 * Mostly copied from JRE Long.toString(long, int).
	 * @see Long#toString(long, int)
	 */
   private static String toStringStandardBase(long value, final int radix, final boolean isNegative)
	{
   	char[] buf = new char[65];
      int charPos = 64;

      if (!isNegative) {
      	value = -value;
      }

      while (value <= -radix) {
          buf[charPos--] = base62Digits[(int)(-(value % radix))];
          value = value / radix;
      }
      buf[charPos] = base62Digits[(int)(-value)];

      if (isNegative) {
          buf[--charPos] = '-';
      }

      return new String(buf, charPos, (65 - charPos));
	}

	private static String toStringBase1(long value, final boolean isNegative)
	{
		if(0 == value) return "";
		//must check for Long.Min here to avoid 2s complement issue
		if(Long.MIN_VALUE == value) throw new IllegalArgumentException(value + " in base 1 would exceed max string length");
		String sign = "";
		if (isNegative)
		{
			if(-Integer.MAX_VALUE == value) throw new IllegalArgumentException(value + " in base 1 would exceed max string length");
			sign = "-";
			value = Math.abs(value);
		}
		if(value > Integer.MAX_VALUE) throw new IllegalArgumentException(value + " in base 1 would exceed max string length");

		//I'm not checking for array overhead such as -8 since it is implementation dependent
		char[] ones = new char[(int)value];
		Arrays.fill(ones, '1');
		return (sign + String.valueOf(ones));
	}

   /**
    * Parses the inputString as a signed long in the radix specified.
    * See toString(long, int) for a description of legal characters per radix.
    * Note some differences from Long.parseLong:
    * <ul>
    * <li>base 1 is allowed to have inputString be the empty string (with an optional leading - or +).
    * For any other radix UncheckedNumberFormatException is thrown (same behavior).</li>
    * <li>this method is case sensitive (in order to support radix &gt; 36).</li>
    * <li>the fullwidth Latin letters are not supported (because they are different characters).</li>
    * </ul>
    *
    * @param inputString the String to be parsed
    * @param radix the number base
    * @return the long that inputString represents
    *
    * @throws NullPointerException if inputString is null
    * @throws NumericOverflowException if inputString represents a number greater than a Long can represent
    * @throws UncheckedNumberFormatException excluding a leading + or - if inputString is empty (and not base 1)
    * or contains illegal characters for that radix
    * @throws IllegalArgumentException {@code if(radix > 62 || radix < 1)}
    *
    * @see Long#parseLong(String, int)
    * @see #toString(long, int) toString(long, int) for a description of legal characters per radix
    */
	public static long parseLong(final String inputString, final int radix)
	{
	   Objects.requireNonNull(inputString, "inputString");
   	enforceStandardRadix(radix);

	   if (1 == radix)
   	{
   		//should be faster than parseLongStandardBase. Also this handles the special case that "" is 0 in base 1
	   	if(inputString.isEmpty()) return 0;
	   	if(!inputString.matches("^[+-]?1*$")) throw UncheckedNumberFormatException.forInputString(inputString, radix);
   		if(inputString.charAt(0) == '-') return -(inputString.length()-1);  //these 2 might return 0
   		if(inputString.charAt(0) == '+') return (inputString.length()-1);
   		return inputString.length();
   	}

      return parseLongStandardBase(inputString, radix);
   }

	/**
	 * Mostly copied from JRE Long.parseLong(String, int).
	 * @see Long#parseLong(String, int)
	 */
   private static long parseLongStandardBase(final String inputString, final int radix)
	{
	   long result = 0;
	   final boolean isNegative = inputString.charAt(0) == '-';
      int i = 0;
	   final int inputLength = inputString.length();
      long limit = -Long.MAX_VALUE;
      long multmin;
      int digit;

	   if(inputLength == 0) throw UncheckedNumberFormatException.forInputString(inputString, radix);

	   char firstChar = inputString.charAt(0);
       if (firstChar < '0') { // Possible leading "+" or "-"
           if (firstChar == '-') {
               limit = Long.MIN_VALUE;
           } else if (firstChar != '+')
               throw UncheckedNumberFormatException.forInputString(inputString, radix);

           if (inputLength == 1) // Cannot have lone "+" or "-"
               throw UncheckedNumberFormatException.forInputString(inputString, radix);
           i++;
       }
       multmin = limit / radix;
       while (i < inputLength) {
           // Accumulating negatively avoids surprises near MAX_VALUE
           digit = getDigitValue(inputString.charAt(i++),radix);
           if (digit < 0) {
               throw UncheckedNumberFormatException.forInputString(inputString, radix);
           }
           if (result < multmin) {
               throw new NumericOverflowException("radix: "+radix+" input string: \"" + inputString + "\"");
           }
           result *= radix;
           if (result < limit + digit) {
              throw new NumericOverflowException("radix: "+radix+" input string: \"" + inputString + "\"");
           }
           result -= digit;
       }
	   return isNegative ? result : -result;
	}

   /**
    * <p>Returns the numeric value of the character {@code digit} in the
    * specified radix. If the character is not a valid digit in the specified radix, -1 is returned.
    * Note that unlike Character.digit the fullwidth Latin letters are not supported (because they are different characters).</p>
    *
    * <p><b>Special case</b>: Base 1 uses the character '1' instead of '0' and returns 1 or -1 accordingly.</p>
    *
    * @param digit the character to be converted
    * @param radix the number base
    * @return the numeric value of digit or -1
    * @throws IllegalArgumentException {@code if(radix > 62 || radix < 1)}
    * @see Character#digit(char, int)
    */
   public static int getDigitValue(final char digit, final int radix)
   {
   	enforceStandardRadix(radix);

   	if (1 == radix)
   	{
   		//special case: base 1 uses '1' instead of '0' and never returns 0
   		if('1' == digit) return 1;
   		return -1;
   	}

   	//why is there no Arrays.indexOf?
   	for (int i = 0; i < base62Digits.length && i < radix; i++)
		{
			if(digit == base62Digits[i]) return i;
		}

   	return -1;
   }

	private static void enforceStandardRadix(final int radix)
	{
		if(radix < 1) throw new IllegalArgumentException("radix < 1 (was " + radix + ")");
   	if(radix > 62) throw new IllegalArgumentException("radix > 62 (was " + radix + ")");
	}

}
