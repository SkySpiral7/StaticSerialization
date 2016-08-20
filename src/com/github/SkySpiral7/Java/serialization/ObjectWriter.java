package com.github.SkySpiral7.Java.serialization;

import java.io.Closeable;
import java.io.File;
import java.io.Flushable;
import java.io.Serializable;
import java.nio.charset.StandardCharsets;

import com.github.SkySpiral7.Java.util.FileIoUtil;

public class ObjectWriter implements Closeable, Flushable
{
	private final ObjectRegistry registry = new ObjectRegistry();
	private final File destination;

	public ObjectWriter(final File destination)
	{
		this.destination = destination;

		//start by clearing the file so that all writes can append (also this is fail fast to prove that writing is possible)
		FileIoUtil.writeToFile(destination, "");
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
		//TODO: for now it doesn't allow arrays
		writeOverhead(data);
		if (data == null) return;
		if(tryWritePrimitive(data)) return;

		if (data instanceof String)
		{
			final String castedData = (String) data;
			final byte[] writeMe = castedData.getBytes(StandardCharsets.UTF_8);
			writeBytes(writeMe.length, 4);
			FileIoUtil.writeToFile(destination, writeMe, true);
			return;
		}

		if (data instanceof StaticSerializable)
		{
			final StaticSerializable castedData = (StaticSerializable) data;
			castedData.writeToStream(this);
			return;
		}

		throw new IllegalArgumentException("Couldn't serialize object of class " + data.getClass().getName());
	}
	/**
	 * @return true if data was written (which means data was primitive)
	 */
	private boolean tryWritePrimitive(final Object data)
	{
		//data.getClass().isPrimitive() is useless because data can only be a box
		if (data instanceof Byte)
		{
			writeBytes((byte) data, 1);
			return true;
		}
		if (data instanceof Short)
		{
			writeBytes((short) data, 2);
			return true;
		}
		if (data instanceof Integer)
		{
			writeBytes((int) data, 4);
			return true;
		}
		if (data instanceof Long)
		{
			writeBytes((long) data, 8);
			return true;
		}
		if (data instanceof Float)
		{
			final int castedData = Float.floatToIntBits((float) data);
			//intentionally normalizes NaN
			writeBytes(castedData, 4);
			return true;
		}
		if (data instanceof Double)
		{
			long castedData = Double.doubleToLongBits((double) data);
			//intentionally normalizes NaN
			writeBytes(castedData, 8);
			return true;
		}
		if (data instanceof Boolean)
		{
			if((boolean) data) writeBytes(1, 1);  //write true
			else writeBytes(0, 1);
			return true;
		}
		if (data instanceof Character)
		{
			writeBytes((char) data, 2);
			return true;
		}

		return false;
	}
	private void writeOverhead(final Object data)
	{
		if (data != null)
		{
			final String className = data.getClass().getName();
			//can't use recursion to write the string because that's endless and needs different format
			final byte[] writeMe = className.getBytes(StandardCharsets.UTF_8);
			FileIoUtil.writeToFile(destination, writeMe, true);
		}
		writeBytes('|', 1);
		//instead of size then string have the string terminated by | since this saves 3 bytes and class names can't contain |
		//if data is null then class name will be the empty string
	}

	public void writeSerializable(final Serializable data){}  //TODO: unchecked/unsafe and difficult to implement
	public void writeFieldsReflectively(final Object data){}  //TODO: stub

	public ObjectRegistry getObjectRegistry(){return registry;}
}
