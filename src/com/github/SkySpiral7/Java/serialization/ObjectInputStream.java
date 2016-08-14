package com.github.SkySpiral7.Java.serialization;

import java.io.Closeable;
import java.io.File;
import java.io.Flushable;
import java.nio.charset.StandardCharsets;
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
	 * TODO: Currently does nothing. Placeholder for later.
	 */
	@Override
   public void flush(){}
	/**
	 * TODO: Currently does nothing. Placeholder for later.
	 */
	@Override
   public void close(){}

	private byte[] readBytes(final int byteCount)
	{
		final byte[] result = new byte[byteCount];
		if (!hasData(byteCount)) throw new IllegalStateException("expeceted " + byteCount + " bytes, found "
				+ remainingBytes() + " bytes");
		for (int i = 0; i < byteCount; ++i)
		{
			result[i] = source[sourceIndex];
			++sourceIndex;
		}
		return result;
	}

	public boolean hasData()
	{
		return (sourceIndex < source.length);
	}
	public boolean hasData(final int byteCount)
	{
		return (byteCount <= remainingBytes());
	}
	public int remainingBytes()
	{
		return (source.length - sourceIndex);
	}

	public Object readObject(){return readObject(Object.class);}
	@SuppressWarnings("unchecked")
	public <T> T readObject(final Class<T> expectedClass)
	{
		Objects.requireNonNull(expectedClass);
		if (!hasData()) throw new IllegalStateException("stream is empty");
		//TODO: for now it doesn't allow array, custom, or overhead
		T result = null;

		result = readPrimitive(expectedClass);
		if(result != null) return result;

		if (String.class.equals(expectedClass))
		{
			final int stringByteLength = BitWiseUtil.bigEndianBytesToInteger(readBytes(4));
			final byte[] data = readBytes(stringByteLength);
			return (T) new String(data, StandardCharsets.UTF_8);
		}

		return null;
	}

	@SuppressWarnings("unchecked")
	private <T> T readPrimitive(final Class<T> expectedClass)
	{
		//expectedClass.isPrimitive() is useless because this method also checks boxes
		if (Byte.class.equals(expectedClass) || byte.class.equals(expectedClass)) { return (T) (Byte) readBytes(1)[0]; }
		if (Short.class.equals(expectedClass) || short.class.equals(expectedClass))
		{
			final byte[] data = readBytes(2);
			final int result = ((data[0] & 0xff) << 8) | (data[1] & 0xff);
			return (T) (Short) (short) result;
		}
		if (Integer.class.equals(expectedClass) || int.class.equals(expectedClass))
		{
			final byte[] data = readBytes(4);
			return (T) (Integer) BitWiseUtil.bigEndianBytesToInteger(data);
		}
		if (Long.class.equals(expectedClass) || long.class.equals(expectedClass))
		{
			final byte[] data = readBytes(8);
			return (T) (Long) BitWiseUtil.bigEndianBytesToLong(data);
		}
		if (Float.class.equals(expectedClass) || float.class.equals(expectedClass))
		{
			final byte[] data = readBytes(4);
			final int intData = BitWiseUtil.bigEndianBytesToInteger(data);
			return (T) (Float) Float.intBitsToFloat(intData);
		}
		if (Double.class.equals(expectedClass) || double.class.equals(expectedClass))
		{
			final byte[] data = readBytes(8);
			final long longData = BitWiseUtil.bigEndianBytesToLong(data);
			return (T) (Double) Double.longBitsToDouble(longData);
		}
		if (Boolean.class.equals(expectedClass) || boolean.class.equals(expectedClass))
		{
			final byte data = readBytes(1)[0];
			if (data == 1) return (T) Boolean.TRUE;
			return (T) Boolean.FALSE;
		}
		if (Character.class.equals(expectedClass) || char.class.equals(expectedClass))
		{
			final byte[] data = readBytes(2);
			final int intData = ((data[0] & 0xff) << 8) | (data[1] & 0xff);
			return (T) (Character) (char) (short) intData;
		}

		return null;
	}

	public Object readSerializable(){return null;}  //TODO: unchecked/unsafe and difficult to implement
	public void readFieldsReflectively(final Object instance){}  //TODO: stub

	public ObjectRegistry getObjectRegistry(){return registry;}
}
