package com.github.SkySpiral7.Java.util;

public class BitHelper {
	private BitHelper(){}

    public static final long HIGH_64 = 0xFFFF_FFFF__FFFF_FFFFL;

    public static boolean isPowerOf2(long x){return ((x & -x) == x);}

    public static long getLowNBits(long value, int nBitsToKeep)
    {
    	if(nBitsToKeep > 64) throw new IllegalArgumentException();
    	if(nBitsToKeep == 64) return value;
    	int nBitsRemoved = 64 - nBitsToKeep;
    	long bitMask = (HIGH_64 >>> nBitsRemoved);
    	return (value & bitMask);
    }

    public static long getHighNBits(long value, int nBitsToKeep)
    {
    	if(nBitsToKeep > 64) throw new IllegalArgumentException();
    	if(nBitsToKeep == 64) return value;
    	int nBitsRemoved = 64 - nBitsToKeep;
    	long bitMask = (HIGH_64 << nBitsRemoved);
    	return (value & bitMask);
    }

    public static int getHighNBits(int value, int nBitsToKeep)
    {
    	if(nBitsToKeep > 32) throw new IllegalArgumentException();
    	if(nBitsToKeep == 32) return value;
    	int nBitsRemoved = 32 - nBitsToKeep;
    	int bitMask = (int) HIGH_64;
    	bitMask <<= nBitsRemoved;
    	return (value & bitMask);
    }

    public static short getHighNBits(short value, int nBitsToKeep)
    {
    	if(nBitsToKeep > 16) throw new IllegalArgumentException();
    	if(nBitsToKeep == 16) return value;
    	int nBitsRemoved = 16 - nBitsToKeep;
    	short bitMask = (short) HIGH_64;
    	bitMask <<= nBitsRemoved;
    	return ((short) (value & bitMask));
    }

    public static byte getHighNBits(byte value, int nBitsToKeep)
    {
    	if(nBitsToKeep > 8) throw new IllegalArgumentException();
    	if(nBitsToKeep == 8) return value;
    	int nBitsRemoved = 8 - nBitsToKeep;
    	byte bitMask = (byte) HIGH_64;
    	bitMask <<= nBitsRemoved;
    	return ((byte) (value & bitMask));
    }

    public static long multiplyBy2PowerN(long value, int nPower){return (value << nPower);}
    public static long divideBy2PowerN(long value, int nPower){return (value >> nPower);}

}
