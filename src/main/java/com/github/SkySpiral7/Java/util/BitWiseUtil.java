package com.github.SkySpiral7.Java.util;

public class BitWiseUtil
{
   private BitWiseUtil(){}

   public static final long HIGH_64 = -1;  //== 0xFFFF_FFFF__FFFF_FFFFL

   public static boolean isPowerOf2(long x){return ((x & -x) == x);}  //also works for Long.MIN_VALUE and negative but maybe not unsigned

   public static boolean isEven(long x){return ((x & 1) == 0);}

   public static boolean isOdd(long x){return ((x & 1) == 1);}  //these work for negative and unsigned values

   public static long getLowestNBits(long value, int nBitsToKeep)
   {
      if (nBitsToKeep > 64) throw new IllegalArgumentException();
      if (nBitsToKeep == 64) return value;
      if (nBitsToKeep == 0) return 0;
      int nBitsRemoved = 64 - nBitsToKeep;
      long bitMask = (HIGH_64 >>> nBitsRemoved);
      return (value & bitMask);
   }

   public static long getHighestNBits(long value, int nBitsToKeep)
   {
      if (nBitsToKeep > 64) throw new IllegalArgumentException();
      if (nBitsToKeep == 64) return value;
      if (nBitsToKeep == 0) return 0;
      int nBitsRemoved = 64 - nBitsToKeep;
      long bitMask = (HIGH_64 << nBitsRemoved);
      return (value & bitMask);
   }

   public static int getHighestNBits(int value, int nBitsToKeep)
   {
      if (nBitsToKeep > 32) throw new IllegalArgumentException();
      if (nBitsToKeep == 32) return value;
      if (nBitsToKeep == 0) return 0;
      int nBitsRemoved = 32 - nBitsToKeep;
      int bitMask = (int) HIGH_64;
      bitMask <<= nBitsRemoved;
      return (value & bitMask);
   }

   public static short getHighestNBits(short value, int nBitsToKeep)
   {
      if (nBitsToKeep > 16) throw new IllegalArgumentException();
      if (nBitsToKeep == 16) return value;
      if (nBitsToKeep == 0) return 0;
      int nBitsRemoved = 16 - nBitsToKeep;
      short bitMask = (short) HIGH_64;
      bitMask <<= nBitsRemoved;
      return ((short) (value & bitMask));
   }

   public static byte getHighestNBits(byte value, int nBitsToKeep)
   {
      if (nBitsToKeep > 8) throw new IllegalArgumentException();
      if (nBitsToKeep == 8) return value;
      if (nBitsToKeep == 0) return 0;
      int nBitsRemoved = 8 - nBitsToKeep;
      byte bitMask = (byte) HIGH_64;
      bitMask <<= nBitsRemoved;
      return ((byte) (value & bitMask));
   }

   public static long multiplyByPowerOf2(long value, int exponent){return (value << exponent);}

   public static long divideByPowerOf2(long value, int exponent){return (value >> exponent);}

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
