package com.github.SkySpiral7.Java.serialization;

import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.Flushable;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;

import com.github.SkySpiral7.Java.util.BitWiseUtil;
import com.github.SkySpiral7.Java.util.ClassUtil;
import com.github.SkySpiral7.Java.util.FileIoUtil;

public class ObjectStreamReader implements Closeable, Flushable
{
	private final ObjectReaderRegistry registry = new ObjectReaderRegistry();
	/** Greedy loading the entire file into memory is bad for performance. */
	private final byte[] source;
	/** This is the index of the next byte to be read from source */
	private int sourceIndex = 0;

	public ObjectStreamReader(final File sourceFile)
	{
		source = FileIoUtil.readBinaryFile(sourceFile);
	}

	/**
	 * TODO: Currently does nothing. Placeholder for later.
	 */
	@Override
	public void flush()
	{}

	/**
	 * TODO: Currently does nothing. Placeholder for later.
	 */
	@Override
	public void close()
	{}

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

	public Object readObject()
	{
		return readObject(Object.class);
	}

	@SuppressWarnings("unchecked")
	public <T> T readObject(Class<T> expectedClass)
	{
		Objects.requireNonNull(expectedClass);
		if (!hasData()) throw new IllegalStateException("stream is empty");

		expectedClass = (Class<T>) autoBox(expectedClass);
		//Class Overhead
		{
			final byte firstByte = readBytes(1)[0];
			if (firstByte == '|') return null;
			expectedClass = (Class<T>) readOverhead(expectedClass, firstByte);
		}
		//TODO: for now it doesn't allow array

		{
			final T result = readPrimitive(expectedClass);
			if (result != null) return result;
		}

		if (String.class.equals(expectedClass))
		{
			final int stringByteLength = BitWiseUtil.bigEndianBytesToInteger(readBytes(4));
			final byte[] data = readBytes(stringByteLength);
			return (T) new String(data, StandardCharsets.UTF_8);
		}

		if (StaticSerializableEnumByName.class.isAssignableFrom(expectedClass))
		{
			final String name = readObject(String.class);
			return (T) Enum.valueOf(ClassUtil.cast(expectedClass), name);
		}
		if (StaticSerializableEnumByOrdinal.class.isAssignableFrom(expectedClass)) { return readEnumByOrdinal(expectedClass); }

		if (StaticSerializable.class.isAssignableFrom(expectedClass)) { return readCustomClass(expectedClass); }

		throw new IllegalArgumentException("Don't know how to deserialize class " + expectedClass.getName());
	}

	private Class<?> readOverhead(final Class<?> expectedClass, final byte firstByte)
	{
		final ByteArrayOutputStream data = new ByteArrayOutputStream();
		data.write(firstByte);
		while (true)
		{
			if (!hasData()) throw new IllegalStateException("Header not found");
			final byte thisByte = readBytes(1)[0];
			if (thisByte == '|') break;
			data.write(thisByte);
		}
		final String actualClassName = new String(data.toByteArray(), StandardCharsets.UTF_8);

		try
		{
			final Class<?> actualClass = Class.forName(actualClassName);
			if (!expectedClass.isAssignableFrom(actualClass)) throw new ClassCastException(actualClass.getName()
					+ " can't be cast into " + expectedClass.getName());
			return actualClass;
		}
		catch (final ClassNotFoundException e)
		{
			throw new RuntimeException(e);
		}
	}

	private Class<?> autoBox(final Class<?> expectedClass)
	{
		//expectedClass.isPrimitive() is pointless: just let it fall through
		if (byte.class.equals(expectedClass)) { return Byte.class; }
		if (short.class.equals(expectedClass)) { return Short.class; }
		if (int.class.equals(expectedClass)) { return Integer.class; }
		if (long.class.equals(expectedClass)) { return Long.class; }
		if (float.class.equals(expectedClass)) { return Float.class; }
		if (double.class.equals(expectedClass)) { return Double.class; }
		if (boolean.class.equals(expectedClass)) { return Boolean.class; }
		if (char.class.equals(expectedClass)) { return Character.class; }
		return expectedClass;
	}

	@SuppressWarnings("unchecked")
	private <T> T readPrimitive(final Class<T> expectedClass)
	{
		//expectedClass.isPrimitive() is useless because this method also checks boxes
		if (Byte.class.equals(expectedClass)) { return (T) (Byte) readBytes(1)[0]; }
		if (Short.class.equals(expectedClass))
		{
			final byte[] data = readBytes(2);
			final int result = ((data[0] & 0xff) << 8) | (data[1] & 0xff);
			return (T) (Short) (short) result;
		}
		if (Integer.class.equals(expectedClass))
		{
			final byte[] data = readBytes(4);
			return (T) (Integer) BitWiseUtil.bigEndianBytesToInteger(data);
		}
		if (Long.class.equals(expectedClass))
		{
			final byte[] data = readBytes(8);
			return (T) (Long) BitWiseUtil.bigEndianBytesToLong(data);
		}
		if (Float.class.equals(expectedClass))
		{
			final byte[] data = readBytes(4);
			final int intData = BitWiseUtil.bigEndianBytesToInteger(data);
			return (T) (Float) Float.intBitsToFloat(intData);
		}
		if (Double.class.equals(expectedClass))
		{
			final byte[] data = readBytes(8);
			final long longData = BitWiseUtil.bigEndianBytesToLong(data);
			return (T) (Double) Double.longBitsToDouble(longData);
		}
		if (Boolean.class.equals(expectedClass))
		{
			final byte data = readBytes(1)[0];
			if (data == 1) return (T) Boolean.TRUE;
			return (T) Boolean.FALSE;
		}
		if (Character.class.equals(expectedClass))
		{
			final byte[] data = readBytes(2);
			final int intData = ((data[0] & 0xff) << 8) | (data[1] & 0xff);
			return (T) (Character) (char) (short) intData;
		}

		return null;
	}

	private <T> T readEnumByOrdinal(final Class<T> expectedClass)
	{
		if (!expectedClass.isEnum()) throw new IllegalArgumentException(expectedClass.getName()
				+ " implements StaticSerializableEnumByOrdinal but isn't an enum");

		final int ordinal = readObject(int.class);
		final Method method;
		try
		{
			//public static Enum[] values()
			method = expectedClass.getDeclaredMethod("values");
		}
		catch (final NoSuchMethodException e)
		{
			throw new RuntimeException("This can't be thrown", e);  //since I already know it is an enum
		}
		catch (final SecurityException e)
		{
			throw new RuntimeException("Couldn't deserialize", e);  //it's too specific to test
		}

		final Enum<?>[] values;
		try
		{
			values = Enum[].class.cast(method.invoke(null));
		}
		catch (final IllegalAccessException | InvocationTargetException | IllegalArgumentException e)
		{
			throw new RuntimeException("This can't be thrown", e);
			//since values is public static, doesn't throw, and I know I'm giving it the right args (none)
		}

		//TODO: create a class for an unchecked StreamCorruptedException and throw that instead of IllegalStateException
		if (values.length <= ordinal) throw new IllegalStateException(String.format(
				"%s[%d] doesn't exist. Actual length: %d", expectedClass.getName(), ordinal, values.length));

		return ClassUtil.cast(values[ordinal]);
	}

	private <T> T readCustomClass(final Class<T> expectedClass)
	{
		final Method method;
		try
		{
			//public static T readFromStream(ObjectStreamReader reader)
			method = expectedClass.getDeclaredMethod("readFromStream", ObjectStreamReader.class);
		}
		catch (final NoSuchMethodException e)
		{
			throw new IllegalStateException(expectedClass.getName()
					+ " implements StaticSerializable but doesn't define readFromStream", e);
		}
		catch (final SecurityException e)
		{
			throw new RuntimeException("Couldn't deserialize", e);  //it's too specific to test
		}

		if (!Modifier.isPublic(method.getModifiers()) || !Modifier.isStatic(method.getModifiers())) { throw new IllegalStateException(
				expectedClass.getName() + ".readFromStream must be public static"); }

		try
		{
			return ClassUtil.cast(method.invoke(null, this));
		}
		catch (final IllegalAccessException | IllegalArgumentException e)
		{
			throw new RuntimeException("This can't be thrown", e);
			//since I already know it is public static and I know I'm giving it the right args
			//(because otherwise it wouldn't have been found)
		}
		catch (final InvocationTargetException e)
		{
			throw new RuntimeException("Couldn't deserialize", e);
		}
	}

	public Serializable readSerializable()
	{
		//TODO: unchecked/unsafe and difficult to implement
		return null;
	}

	public void readFieldsReflectively(final Object instance)
	{
		final List<Field> allSerializableFields = SerializationUtil.getAllSerializableFields(instance.getClass());
		allSerializableFields.forEach(field -> {
			field.setAccessible(true);
			try
			{
				field.set(instance, this.readObject());  //will auto-cast
			}
			catch (final IllegalAccessException e)
			{
				throw new RuntimeException(e);
			}
		});
	}

	public ObjectReaderRegistry getObjectRegistry()
	{
		return registry;
	}
}
