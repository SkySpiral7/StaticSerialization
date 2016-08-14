package com.github.SkySpiral7.Java.serialization;

import java.io.Closeable;
import java.io.File;
import java.io.Flushable;
import java.util.Objects;

import com.github.SkySpiral7.Java.util.BitWiseUtil;
import com.github.SkySpiral7.Java.util.FileIoUtil;

public class ObjectInputStream implements Closeable, Flushable
{
	/**This is cached so that the value can't change for this stream.*/
	private final boolean generateClassNameOverhead;
	private final ObjectRegistry registry = new ObjectRegistry();
	/**Greedy loading the entire file into memory is bad for performance.*/
	private final byte[] source;
	/**This is the index of the next byte to be read from source*/
	private int sourceIndex = 0;

	public ObjectInputStream(final File sourceFile)
	{
		generateClassNameOverhead = StaticSerializableConfig.generateClassNameOverhead;
		source = FileIoUtil.readBinaryFile(sourceFile);
	}

	/**
	 * Currently does nothing. Placeholder for later.
	 */
	@Override
   public void flush(){}
	/**
	 * Currently does nothing. Placeholder for later.
	 */
	@Override
   public void close(){}

	private byte[] readBytes(final int byteCount)
	{
		final byte[] result = new byte[byteCount];
		for (int i = 0; hasData() && i < byteCount; ++i)
		{
			result[i] = source[sourceIndex];
			++sourceIndex;
		}
		return result;
	}

	public boolean hasData()
	{
		//can't call hasData(byte.class) because of overhead mismatch
		return (sourceIndex < source.length);
	}
	private boolean hasData(final int byteCount)
	{
		return ((sourceIndex + byteCount) <= source.length);
	}

	public boolean hasData(final Class<?> expectedClass){return false;}
	public int remainingBytes(){return 0;}

	public Object readObject(){return readObject(Object.class);}
	public <T> T readObject(final Class<T> expectedClass)
	{
		Objects.requireNonNull(expectedClass);
		//for now it doesn't allow array, string, custom, or overhead
		//also for now ignore remaining data length
		T result = null;
		result = readPrimitive(expectedClass);
		return result;
	}

	@SuppressWarnings("unchecked")
	private <T> T readPrimitive(final Class<T> expectedClass)
	{
		//expectedClass.isPrimitive() is useless because this method also checks boxes
		if (expectedClass.isAssignableFrom(Byte.class)) { return (T) (Byte) readBytes(1)[0]; }
		if (expectedClass.isAssignableFrom(Short.class))
		{
			final byte[] data = readBytes(2);
			final int result = ((data[0] & 0xff) << 8) | (data[1] & 0xff);
			return (T) (Short) (short) result;
		}
		if (expectedClass.isAssignableFrom(Integer.class))
		{
			final byte[] data = readBytes(4);
			return (T) (Integer) BitWiseUtil.bigEndianBytesToInteger(data);
		}
		if (expectedClass.isAssignableFrom(Long.class))
		{
			final byte[] data = readBytes(8);
			return (T) (Long) BitWiseUtil.bigEndianBytesToLong(data);
		}
		if (expectedClass.isAssignableFrom(Float.class))
		{
			final byte[] data = readBytes(4);
			final int intData = BitWiseUtil.bigEndianBytesToInteger(data);
			return (T) (Float) Float.intBitsToFloat(intData);
		}
		if (expectedClass.isAssignableFrom(Double.class))
		{
			final byte[] data = readBytes(8);
			final long longData = BitWiseUtil.bigEndianBytesToLong(data);
			return (T) (Double) Double.longBitsToDouble(longData);
		}
		if (expectedClass.isAssignableFrom(Boolean.class))
		{
			final byte data = readBytes(1)[0];
			if (data == 1) return (T) Boolean.TRUE;
			return (T) Boolean.FALSE;
		}
		if (expectedClass.isAssignableFrom(Character.class))
		{
			final byte[] data = readBytes(2);
			final int intData = ((data[0] & 0xff) << 8) | (data[1] & 0xff);
			return (T) (Character) (char) (short) intData;
		}

		return null;
	}

	public Object readSerializable(){return null;}  //unchecked/unsafe and difficult to implement
	public void readFieldsReflectively(final Object instance){}

	public ObjectRegistry getObjectRegistry(){return registry;}
}
