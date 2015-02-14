package com.github.SkySpiral7.Java.util;

public class BitWiseUtil {
	private BitWiseUtil(){}

    public static final long HIGH_64 = -1;  //== 0xFFFF_FFFF__FFFF_FFFFL

    public static boolean isPowerOf2(long x){return ((x & -x) == x);}  //also works for Long.MIN_VALUE but maybe not unsigned
    public static boolean isEven(long x){return ((x & 1) == 0);}
    public static boolean isOdd(long x){return ((x & 1) == 1);}  //these work for negative and unsigned values

    public static long getLowestNBits(long value, int nBitsToKeep)
    {
    	if(nBitsToKeep > 64) throw new IllegalArgumentException();
    	if(nBitsToKeep == 64) return value;
    	if(nBitsToKeep == 0) return 0;
    	int nBitsRemoved = 64 - nBitsToKeep;
    	long bitMask = (HIGH_64 >>> nBitsRemoved);
    	return (value & bitMask);
    }

    public static long getHighestNBits(long value, int nBitsToKeep)
    {
    	if(nBitsToKeep > 64) throw new IllegalArgumentException();
    	if(nBitsToKeep == 64) return value;
    	if(nBitsToKeep == 0) return 0;
    	int nBitsRemoved = 64 - nBitsToKeep;
    	long bitMask = (HIGH_64 << nBitsRemoved);
    	return (value & bitMask);
    }

    public static int getHighestNBits(int value, int nBitsToKeep)
    {
    	if(nBitsToKeep > 32) throw new IllegalArgumentException();
    	if(nBitsToKeep == 32) return value;
    	if(nBitsToKeep == 0) return 0;
    	int nBitsRemoved = 32 - nBitsToKeep;
    	int bitMask = (int) HIGH_64;
    	bitMask <<= nBitsRemoved;
    	return (value & bitMask);
    }

    public static short getHighestNBits(short value, int nBitsToKeep)
    {
    	if(nBitsToKeep > 16) throw new IllegalArgumentException();
    	if(nBitsToKeep == 16) return value;
    	if(nBitsToKeep == 0) return 0;
    	int nBitsRemoved = 16 - nBitsToKeep;
    	short bitMask = (short) HIGH_64;
    	bitMask <<= nBitsRemoved;
    	return ((short) (value & bitMask));
    }

    public static byte getHighestNBits(byte value, int nBitsToKeep)
    {
    	if(nBitsToKeep > 8) throw new IllegalArgumentException();
    	if(nBitsToKeep == 8) return value;
    	if(nBitsToKeep == 0) return 0;
    	int nBitsRemoved = 8 - nBitsToKeep;
    	byte bitMask = (byte) HIGH_64;
    	bitMask <<= nBitsRemoved;
    	return ((byte) (value & bitMask));
    }

    public static long multiplyBy2PowerN(long value, int nPower){return (value << nPower);}
    public static long divideBy2PowerN(long value, int nPower){return (value >> nPower);}

}
