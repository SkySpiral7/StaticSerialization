package com.github.SkySpiral7.Java.serialization;

import java.io.Closeable;
import java.io.File;
import java.io.Flushable;
import java.io.Serializable;
import java.util.Objects;

import com.github.SkySpiral7.Java.util.FileIoUtil;

public class ObjectOutputStream implements Closeable, Flushable
{
	private final File destination;
	private final ObjectRegistry registry = new ObjectRegistry();

	public ObjectOutputStream(final File destination)
	{
		this.destination = destination;

		//start by clearing the file so that all writes can append (also this is fail fast to prove that writing is possible)
		FileIoUtil.writeToFile(destination, "");
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

	private void writeBytes(long data, final int byteCount)
	{
		final byte[] writeMe = new byte[byteCount];
		for (int i = (byteCount - 1); i >= 0; --i)
		{
			//the array is reversed so that it is in big endian
			writeMe[i] = (byte) (data & 0xFF);
			data >>>= 8;
		}
		FileIoUtil.writeToFile(destination, writeMe, true);
	}

	//for now ignore overloading for all primitives and array stuff
	public void writeObject(final Object data)
	{
		Objects.requireNonNull(data);  //for now
		//for now it also doesn't allow arrays
		if (StaticSerializable.generateClassNameOverhead)
		{
			//TODO: create overhead
			//can't use recursion because that's endless
			//writeByte(data.getClass().getName());
		}

		if (data instanceof Byte)
		{
			writeBytes((byte) data, 1);
			return;
		}
		if (data instanceof Short)
		{
			writeBytes((short) data, 2);
			return;
		}
		if (data instanceof Integer)
		{
			writeBytes((int) data, 4);
			return;
		}
		if (data instanceof Long)
		{
			writeBytes((long) data, 8);
			return;
		}
		if (data instanceof Float)
		{
			final int castedData = Float.floatToIntBits((float) data);
			//intentionally normalizes NaN
			writeBytes(castedData, 4);
			return;
		}
		if (data instanceof Double)
		{
			long castedData = Double.doubleToLongBits((double) data);
			//intentionally normalizes NaN
			writeBytes(castedData, 8);
			return;
		}
		if (data instanceof Boolean)
		{
			if((boolean) data) writeBytes(1, 1);  //write true
			else writeBytes(0, 1);
			return;
		}
		if (data instanceof Character)
		{
			writeBytes((char) data, 2);
			return;
		}
		/*if (data instanceof String)
		{
			final String castedData = (String) data;
			final byte[] writeMe = castedData.getBytes(StandardCharsets.UTF_8);
			//TODO: must also write the length so that it can be read
			writeBytes(writeMe);
			return;
		}*/

		if (data instanceof StaticSerializable)
		{
			final StaticSerializable<?> castedData = (StaticSerializable<?>) data;
			castedData.writeToStream(this);
			return;
		}

		throw new IllegalArgumentException("Couldn't serialize object of class " + data.getClass().getName());
	}
	public void writeSerializable(final Serializable data){}  //unchecked/unsafe and difficult to implement
	public void writeFieldsReflectively(final Object data){}

	public ObjectRegistry getObjectRegistry(){return registry;}
}
