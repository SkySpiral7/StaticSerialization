package com.github.skySpiral7.java.staticSerialization.util;

public class BitWiseUtil
{

   public byte[] toBigEndianBytes(long data, final int byteCount)
   {
      final byte[] result = new byte[byteCount];
      for (int i = (byteCount - 1); i >= 0; --i)
      {
         //the array is reversed so that it is in big endian
         result[i] = (byte) (data & 0xFF);
         data >>>= 8;
      }
      return result;
   }

   public int bigEndianBytesToInteger(final byte[] input)
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

   public long bigEndianBytesToLong(final byte[] input)
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
