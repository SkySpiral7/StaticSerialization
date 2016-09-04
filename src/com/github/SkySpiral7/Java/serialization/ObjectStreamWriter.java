package com.github.SkySpiral7.Java.serialization;

import com.github.SkySpiral7.Java.AsynchronousFileAppender;
import com.github.SkySpiral7.Java.util.FileIoUtil;

import java.io.*;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.List;

public class ObjectStreamWriter implements Closeable, Flushable
{
	private final ObjectWriterRegistry registry = new ObjectWriterRegistry();
	private final AsynchronousFileAppender fileAppender;

	public ObjectStreamWriter(final File destination)
	{
		//start by clearing the file so that all writes can append (also this is fail fast to prove that writing is possible)
		FileIoUtil.writeToFile(destination, "");  //must do before fileAppender is created so that the file won't be locked
		fileAppender = new AsynchronousFileAppender(destination);
	}

	/**
	 * @see AsynchronousFileAppender#flush()
	 */
	@Override
	public void flush()
	{
		fileAppender.flush();
	}

	/**
	 * @see AsynchronousFileAppender#close()
	 */
	@Override
	public void close()
	{
		fileAppender.close();
	}

	private void writeBytes(long data, final int byteCount)
	{
		final byte[] writeMe = new byte[byteCount];
		for (int i = (byteCount - 1); i >= 0; --i)
		{
			//the array is reversed so that it is in big endian
			writeMe[i] = (byte) (data & 0xFF);
			data >>>= 8;
		}
		fileAppender.append(writeMe);
	}

	//for now ignore overloading for all primitives and array stuff
	public void writeObject(final Object data)
	{
		//TODO: for now it doesn't allow arrays
		writeOverhead(data);
		if (data == null) return;
		if (data.getClass().isAnnotationPresent(GenerateId.class))
		{
			if (registry.getId(data) != null) {
				registry.writeId(data, this);
				//if already exists then write the id and stop
				return;
			}
			//else create an id, write it, and continue writing the object
			registry.registerObject(data);
			registry.writeId(data, this);
		}
		else if (tryWritePrimitive(data)) return;

		if (data instanceof String)
		{
			final String castedData = (String) data;
			final byte[] writeMe = castedData.getBytes(StandardCharsets.UTF_8);
			writeBytes(writeMe.length, 4);
			fileAppender.append(writeMe);
			return;
		}

		if (data instanceof StaticSerializableEnumByName)
		{
			final Enum<?> castedData = (Enum<?>) data;
			writeObject(castedData.name());
			return;
		}
		if (data instanceof StaticSerializableEnumByOrdinal)
		{
			final Enum<?> castedData = (Enum<?>) data;
			writeObject(castedData.ordinal());
			return;
		}

		if (data instanceof StaticSerializable)
		{
			final StaticSerializable castedData = (StaticSerializable) data;
			castedData.writeToStream(this);
			return;
		}
		if (data instanceof Serializable)
		{
			final Serializable castedData = (Serializable) data;
			final byte[] serializedData = javaSerialize(castedData);
			this.writeBytes(serializedData.length, 4);
			fileAppender.append(serializedData);
			return;
		}

		throw new IllegalArgumentException("Don't know how to serialize object of class " + data.getClass().getName());
	}

	static byte[] javaSerialize(final Serializable castedData)
	{
		final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(512);
		try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
		{
			out.writeObject(castedData);
		}
		catch (final IOException ex)
		{
			throw new RuntimeException(ex);
		}
		return byteStream.toByteArray();
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
			if ((boolean) data) writeBytes(1, 1);  //write true
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
			fileAppender.append(writeMe);
		}
		writeBytes('|', 1);
		//instead of size then string have the string terminated by | since this saves 3 bytes and class names can't contain |
		//if data is null then class name will be the empty string
		//TODO: better compression: &Z see Class.getName
	}

	public void writeFieldsReflectively(final Object data)
	{
		final List<Field> allSerializableFields = SerializationUtil.getAllSerializableFields(data.getClass());
		allSerializableFields.forEach(field -> {
			field.setAccessible(true);
			try
			{
				this.writeObject(field.get(data));
			}
			catch (final IllegalAccessException e)
			{
				throw new RuntimeException("This can't be thrown.", e);
				//since I would've gotten SecurityException from setAccessible(true)
			}
		});
	}

	public ObjectWriterRegistry getObjectRegistry()
	{
		return registry;
	}
}
