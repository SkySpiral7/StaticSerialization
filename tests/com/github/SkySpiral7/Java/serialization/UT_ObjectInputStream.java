package com.github.SkySpiral7.Java.serialization;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import org.junit.Before;
import org.junit.Test;

import com.github.SkySpiral7.Java.util.FileIoUtil;

public class UT_ObjectInputStream
{
	@Before
	public void setUp()
	{
		StaticSerializableConfig.generateClassNameOverhead = false;
	}

	@Test
	public void constructor_throws()
	{
		try
		{
			new ObjectInputStream(new File(".")).close();
		}
		catch (final IllegalArgumentException actual)
		{
			assertEquals("It is not possible to read file contents of a directory", actual.getMessage());
		}
	}

	@Test
	public void readBytes_throw() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readBytes_throw.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 0x0a };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		try
		{
			testObject.readObject(Short.class);
		}
		catch (final IllegalStateException actual)
		{
			assertEquals("expeceted 2 bytes, found 1 bytes", actual.getMessage());
			//this indirectly tests hasData(int) and remainingBytes(). hasData() is tested everywhere
		}

		testObject.close();
	}

	@Test
	public void readObject_throw_nullInput() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_throw_nullInput.", ".txt");
		tempFile.deleteOnExit();

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		boolean didCatch = false;
		try
		{
			testObject.readObject(null);
		}
		catch (final NullPointerException actual)
		{
			didCatch = true;
		}
		assertTrue(didCatch);

		testObject.close();
	}

	@Test
	public void readObject_throw_noData() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_throw_noData.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, new byte[0], false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertFalse(testObject.hasData());
		try
		{
			testObject.readObject(Byte.class);
		}
		catch (final IllegalStateException actual)
		{
			assertEquals("stream is empty", actual.getMessage());
		}

		testObject.close();
	}

	@Test
	public void readObject_throw_unknownClass() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_throw_unknownClass.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 2 };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		try
		{
			testObject.readObject(Object.class);
		}
		catch (final IllegalArgumentException actual)
		{
			assertEquals("Don't know how to deserialize class java.lang.Object", actual.getMessage());
		}

		testObject.close();
	}

	@Test
	public void readObject_byte() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_byte.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 2 };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(2L, testObject.readObject(byte.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_Byte() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_Byte.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 2 };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(2L, testObject.readObject(Byte.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_short() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_short.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 0x0a, (byte) 0xfe };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(0x0afeL, testObject.readObject(short.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_Short() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_Short.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 0x0a, (byte) 0xfe };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(0x0afeL, testObject.readObject(Short.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_int() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_int.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 0x0a, (byte) 0xfe, (byte) 0xba, (byte) 0xbe };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(0x0afe_babeL, testObject.readObject(int.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_Integer() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_Integer.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 0x0a, (byte) 0xfe, (byte) 0xba, (byte) 0xbe };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(0x0afe_babeL, testObject.readObject(Integer.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_long() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_long.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x04, (byte) 0x05, (byte) 0x06,
				(byte) 0x07, (byte) 0x08 };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(0x01020304_05060708L, testObject.readObject(long.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_Long() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_Long.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x04, (byte) 0x05, (byte) 0x06,
				(byte) 0x07, (byte) 0x08 };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(0x01020304_05060708L, testObject.readObject(Long.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_float() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_float.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x04 };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final Float expected = Float.intBitsToFloat(0x01020304);
		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(expected, testObject.readObject(float.class));
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_Float() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_Float.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x04 };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final Float expected = Float.intBitsToFloat(0x01020304);
		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(expected, testObject.readObject(Float.class));
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_double() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_double.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x04, (byte) 0x05, (byte) 0x06,
				(byte) 0x07, (byte) 0x08 };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final Double expected = Double.longBitsToDouble(0x01020304_05060708L);
		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(expected, testObject.readObject(double.class));
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_Double() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_Double.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x04, (byte) 0x05, (byte) 0x06,
				(byte) 0x07, (byte) 0x08 };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final Double expected = Double.longBitsToDouble(0x01020304_05060708L);
		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(expected, testObject.readObject(Double.class));
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_boolean() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_boolean.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 0x00, (byte) 0x01 };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertFalse(testObject.readObject(boolean.class));
		assertTrue(testObject.readObject(boolean.class));
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_Boolean() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_Boolean.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 0x00, (byte) 0x01 };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertFalse(testObject.readObject(Boolean.class));
		assertTrue(testObject.readObject(Boolean.class));
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_char() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_char.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 0x00, (byte) 0x66, (byte) 0x22, (byte) 0x1e };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertEquals('f', testObject.readObject(char.class).charValue());
		assertEquals('\u221E', testObject.readObject(char.class).charValue());  //infinity sign is BMP non-private
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_Character() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_Character.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 0x00, (byte) 0x66, (byte) 0x22, (byte) 0x1e };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertEquals('f', testObject.readObject(Character.class).charValue());
		assertEquals('\u221E', testObject.readObject(Character.class).charValue());  //infinity sign is BMP non-private
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_String() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectInputStream.TempFile.readObject_String.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x04,  //UTF-8 length (int)
				(byte) 0x66, (byte) 0xe2, (byte) 0x88, (byte) 0x9e };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectInputStream testObject = new ObjectInputStream(tempFile);
		assertTrue(testObject.hasData());
		assertEquals("f\u221E", testObject.readObject(String.class));  //infinity sign is BMP (3 UTF-8 bytes) non-private
		assertFalse(testObject.hasData());

		testObject.close();
	}
}
