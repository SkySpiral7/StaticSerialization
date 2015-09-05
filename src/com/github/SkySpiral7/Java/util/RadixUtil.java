package com.github.SkySpiral7.Java.util;

import java.util.Arrays;

public final class RadixUtil {
   private RadixUtil(){}

   /**
    * All possible chars for representing a number as a String up to base 62
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
    *
    * @param value the number that is to be represented
    * @param radix the number base
    * @return a String that represents value in the given number base (radix).
    * @throws IllegalArgumentException for unsupported radix or if the base 1 number will not fit into a String
    * @see Long#toString(long, int)
    */
   public static String toString(long value, final int radix)
   {
   	if(radix < 1) throw new IllegalArgumentException("radix < 1");
   	if(radix > 62) throw new IllegalArgumentException("radix > 62");
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

	private static String toStringStandardBase(long value, final int radix, final boolean isNegative)
	{
		//below was mostly copied from JRE java.lang.Long.toString(long, int)
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
		if(Long.MIN_VALUE == value) throw new IllegalArgumentException("Number would exceed max string length");
		String sign = "";
		if (isNegative)
		{
			sign = "-";
			value = Math.abs(value);
		}
		if(value > Integer.MAX_VALUE) throw new IllegalArgumentException("Number would exceed max string length");
		if(isNegative && value == Integer.MAX_VALUE) throw new IllegalArgumentException("Number would exceed max string length");

		//I'm not checking for array overhead such as -8 since it is implementation dependent
		char[] ones = new char[(int)value];
		Arrays.fill(ones, '1');
		return (sign + String.valueOf(ones));
	}

}
