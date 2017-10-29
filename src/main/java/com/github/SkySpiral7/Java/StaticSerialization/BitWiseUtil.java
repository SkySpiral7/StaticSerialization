package com.github.SkySpiral7.Java.StaticSerialization;

/**
 * Copied from Java repo to lessen dependency.
 *
 * @see com.github.SkySpiral7.Java.util.BitWiseUtil
 */
public enum BitWiseUtil
{
   ;  //no instances

   /**
    * Copied from Java repo to lessen dependency.
    *
    * @see com.github.SkySpiral7.Java.util.BitWiseUtil#bigEndianBytesToLong(byte[])
    */
   public static int bigEndianBytesToInteger(byte[] input)
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

   /**
    * Copied from Java repo to lessen dependency.
    *
    * @see com.github.SkySpiral7.Java.util.BitWiseUtil#bigEndianBytesToLong(byte[])
    */
   public static long bigEndianBytesToLong(byte[] input)
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
