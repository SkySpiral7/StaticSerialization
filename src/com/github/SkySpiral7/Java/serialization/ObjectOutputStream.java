package com.github.SkySpiral7.Java.serialization;

import java.io.Closeable;
import java.io.File;
import java.io.Flushable;
import java.io.Serializable;
import java.nio.charset.StandardCharsets;

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
		//for now it doesn't allow arrays
		writeOverhead(data);
		if (data == null) return;

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
		if (data instanceof String)
		{
			final String castedData = (String) data;
			final byte[] writeMe = castedData.getBytes(StandardCharsets.UTF_16BE);
			//TODO: if possible for reading, use UTF-8 writeMe.length
			writeBytes(castedData.length(), 4);
			FileIoUtil.writeToFile(destination, writeMe, true);
			return;
		}

		if (data instanceof StaticSerializable)
		{
			final StaticSerializable<?> castedData = (StaticSerializable<?>) data;
			castedData.writeToStream(this);
			return;
		}

		throw new IllegalArgumentException("Couldn't serialize object of class " + data.getClass().getName());
	}
	private void writeOverhead(final Object data)
	{
		if (StaticSerializableConfig.generateClassNameOverhead)
		{
			//can't use recursion to write the string because that's endless and needs different format
			String className = Object.class.getName();
			if(data != null) className = data.getClass().getName();

			final byte[] writeMe = className.getBytes(StandardCharsets.UTF_16BE);
			FileIoUtil.writeToFile(destination, writeMe, true);
			writeBytes('|', 2);
			//instead of size then string have the string terminated by | since this saves 2 bytes and class names can't contain |

			//then write the "hasData" boolean
			if (data == null) writeBytes(0, 1);
			else writeBytes(1, 1);
		}
		else if (data == null) throw new IllegalArgumentException(
				"Can't write null without overhead because it would be impossible to read");
	}

	public void writeSerializable(final Serializable data){}  //unchecked/unsafe and difficult to implement
	public void writeFieldsReflectively(final Object data){}

	public ObjectRegistry getObjectRegistry(){return registry;}
}
