package com.github.SkySpiral7.Java.serialization;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import org.junit.Test;

import com.github.SkySpiral7.Java.util.FileIoUtil;

public class UT_ObjectOutputStream
{
	@Test
	public void constructor_throws()
	{
		try
		{
			new ObjectOutputStream(new File(".")).close();
		}
		catch (final IllegalArgumentException actual)
		{
			assertEquals("It is not possible to write to a directory", actual.getMessage());
		}
	}

	@Test
	public void constructor_clears() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectOutputStream.TempFile.constructor_clears.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "test");
		new ObjectOutputStream(tempFile).close();
		assertEquals("", FileIoUtil.readTextFile(tempFile));
	}

	@Test
	public void writeObject_byte() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectOutputStream.TempFile.writeObject_byte.", ".txt");
		tempFile.deleteOnExit();
		final ObjectOutputStream testObject = new ObjectOutputStream(tempFile);
		final Byte data = (byte) 2;

		testObject.writeObject(data);
		assertEquals(2, FileIoUtil.readBinaryFile(tempFile)[0]);

		testObject.close();
	}

	@Test
	public void writeObject_short() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectOutputStream.TempFile.writeObject_short.", ".txt");
		tempFile.deleteOnExit();
		final ObjectOutputStream testObject = new ObjectOutputStream(tempFile);
		final Short data = (short) 0xcafe;
		final byte[] expected = { (byte) 0xca, (byte) 0xfe };

		testObject.writeObject(data);
		assertArrayEquals(expected, FileIoUtil.readBinaryFile(tempFile));

		testObject.close();
	}

	@Test
	public void writeObject_int() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectOutputStream.TempFile.writeObject_int.", ".txt");
		tempFile.deleteOnExit();
		final ObjectOutputStream testObject = new ObjectOutputStream(tempFile);
		final Integer data = 0xcafe_bead;
		final byte[] expected = { (byte) 0xca, (byte) 0xfe, (byte) 0xbe, (byte) 0xad };

		testObject.writeObject(data);
		assertArrayEquals(expected, FileIoUtil.readBinaryFile(tempFile));

		testObject.close();
	}

	@Test
	public void writeObject_long() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectOutputStream.TempFile.writeObject_long.", ".txt");
		tempFile.deleteOnExit();
		final ObjectOutputStream testObject = new ObjectOutputStream(tempFile);
		final Long data = 0xdead_beef__b100_d123L;
		final byte[] expected = { (byte) 0xde, (byte) 0xad, (byte) 0xbe, (byte) 0xef, (byte) 0xb1, (byte) 0x00,
				(byte) 0xd1, (byte) 0x23 };

		testObject.writeObject(data);
		assertArrayEquals(expected, FileIoUtil.readBinaryFile(tempFile));

		testObject.close();
	}

	@Test
	public void writeObject_float() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectOutputStream.TempFile.writeObject_float.", ".txt");
		tempFile.deleteOnExit();
		final ObjectOutputStream testObject = new ObjectOutputStream(tempFile);
		final Float data = Float.intBitsToFloat(0xcafe_bead);
		final byte[] expected = { (byte) 0xca, (byte) 0xfe, (byte) 0xbe, (byte) 0xad };

		testObject.writeObject(data);
		assertArrayEquals(expected, FileIoUtil.readBinaryFile(tempFile));

		testObject.close();
	}

	@Test
	public void writeObject_double() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectOutputStream.TempFile.writeObject_double.", ".txt");
		tempFile.deleteOnExit();
		final ObjectOutputStream testObject = new ObjectOutputStream(tempFile);
		final Double data = Double.longBitsToDouble(0xdead_beef__b100_d123L);
		final byte[] expected = { (byte) 0xde, (byte) 0xad, (byte) 0xbe, (byte) 0xef, (byte) 0xb1, (byte) 0x00,
				(byte) 0xd1, (byte) 0x23 };

		testObject.writeObject(data);
		assertArrayEquals(expected, FileIoUtil.readBinaryFile(tempFile));

		testObject.close();
	}

	@Test
	public void writeObject_boolean() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectOutputStream.TempFile.writeObject_boolean.", ".txt");
		tempFile.deleteOnExit();
		final ObjectOutputStream testObject = new ObjectOutputStream(tempFile);

		testObject.writeObject(true);
		testObject.writeObject(false);
		assertArrayEquals(new byte[] { (byte) 1, 0 }, FileIoUtil.readBinaryFile(tempFile));

		testObject.close();
	}

	@Test
	public void writeObject_char() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectOutputStream.TempFile.writeObject_char.", ".txt");
		tempFile.deleteOnExit();
		final ObjectOutputStream testObject = new ObjectOutputStream(tempFile);

		testObject.writeObject('f');
		assertArrayEquals(new byte[] { (byte) 0x00, (byte) 0x66 }, FileIoUtil.readBinaryFile(tempFile));
		FileIoUtil.writeToFile(tempFile, "");
		//TODO: appending seems to fail for some reason

		testObject.writeObject('\u221E');  //infinity sign is BMP non-private
		assertArrayEquals(new byte[] { (byte) 0x22, (byte) 0x1e }, FileIoUtil.readBinaryFile(tempFile));

		testObject.close();
	}

	@Test
	public void writeObject_custom() throws IOException
	{
		final class Local implements StaticSerializable<Object>
		{
			boolean wasCalled = false;

			@Override
			public void writeToStream(ObjectOutputStream out)
			{
				wasCalled = true;
			}
		}

		final File tempFile = File.createTempFile("UT_ObjectOutputStream.TempFile.writeObject_custom.", ".txt");
		tempFile.deleteOnExit();
		final ObjectOutputStream testObject = new ObjectOutputStream(tempFile);
		final Local data = new Local();

		testObject.writeObject(data);
		assertTrue(data.wasCalled);

		testObject.close();
	}

	@Test
	public void writeObject_throws() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectOutputStream.TempFile.writeObject_throws.", ".txt");
		tempFile.deleteOnExit();
		final ObjectOutputStream testObject = new ObjectOutputStream(tempFile);

		try
		{
			testObject.writeObject(tempFile);
		}
		catch (final IllegalArgumentException actual)
		{
			assertEquals("Couldn't serialize object of class java.io.File", actual.getMessage());
		}

		testObject.close();
	}
}
