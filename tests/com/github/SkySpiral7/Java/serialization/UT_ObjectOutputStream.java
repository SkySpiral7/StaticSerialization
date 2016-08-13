package com.github.SkySpiral7.Java.serialization;

import static org.junit.Assert.assertEquals;

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
		final File tempFile = File.createTempFile("UT_StaticSerializableEnumByName.TempFile.constructor_clears.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "test");
		new ObjectOutputStream(tempFile).close();
		assertEquals("", FileIoUtil.readFromFileAsString(tempFile));
	}

	@Test
	public void write_byte() throws IOException
	{
		final File tempFile = File.createTempFile("UT_StaticSerializableEnumByName.TempFile.write_byte.", ".txt");
		tempFile.deleteOnExit();
		final ObjectOutputStream testObject = new ObjectOutputStream(tempFile);
		testObject.write(0x102);
		testObject.close();
		assertEquals(2, FileIoUtil.readFromFileAsBinary(tempFile)[0]);
	}
}
