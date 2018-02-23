package com.github.skySpiral7.java.staticSerialization.util;

public enum BitWiseUtil
{
   ;  //no instances

   public static int bigEndianBytesToInteger(final byte[] input)
   {
      if (input.length != 4) throw new IllegalArgumentException("expected length 4, got: " + input.length);
      int result = (input[0] & 0xff);
      for (int i = 1; i < input.length; ++i)
      {
         result <<= 8;
         result |= (input[i] & 0xff);
      }
      return result;
   }

   public static long bigEndianBytesToLong(final byte[] input)
   {
      if (input.length != 8) throw new IllegalArgumentException("expected length 8, got: " + input.length);
      long result = (input[0] & 0xff);
      for (int i = 1; i < input.length; ++i)
      {
         result <<= 8;
         result |= (input[i] & 0xff);
      }
      return result;
   }

}
