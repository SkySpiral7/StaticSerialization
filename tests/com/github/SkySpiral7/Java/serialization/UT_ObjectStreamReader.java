package com.github.SkySpiral7.Java.serialization;

import com.github.SkySpiral7.Java.serialization.testClasses.SimpleHappy;
import com.github.SkySpiral7.Java.util.FileIoUtil;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;

import static org.junit.Assert.*;

public class UT_ObjectStreamReader
{
	@Test
	public void constructor_throws()
	{
		try
		{
			new ObjectStreamReader(new File(".")).close();
		}
		catch (final IllegalArgumentException actual)
		{
			assertEquals("It is not possible to read file contents of a directory", actual.getMessage());
		}
	}

	@Test
	public void readBytes_throw() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readBytes_throw.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Short|".getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = { (byte) 0x0a };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		try
		{
			testObject.readObject(Short.class);
		}
		catch (final IllegalStateException actual)
		{
			assertEquals("expected 2 bytes, found 1 bytes", actual.getMessage());
			//this indirectly tests hasData(int) and remainingBytes(). hasData() is tested everywhere
		}

		testObject.close();
	}

	@Test
	public void readObject_throw_nullInput() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_throw_nullInput.", ".txt");
		tempFile.deleteOnExit();

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
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
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_throw_noData.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, new byte[0], false);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
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
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_throw_unknownClass.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Object|".getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = { (byte) 2 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
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
	public void autoBox_byte() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.autoBox_byte.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Byte|".getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = { (byte) 2 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(2L, testObject.readObject(byte.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void autoBox_short() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.autoBox_short.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Short|".getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = { (byte) 0x0a, (byte) 0xfe };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(0x0afeL, testObject.readObject(short.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void autoBox_int() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.autoBox_int.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Integer|".getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = { (byte) 0x0a, (byte) 0xfe, (byte) 0xba, (byte) 0xbe };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(0x0afe_babeL, testObject.readObject(int.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void autoBox_long() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.autoBox_long.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Long|".getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = { (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x04, (byte) 0x05, (byte) 0x06,
				(byte) 0x07, (byte) 0x08 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(0x01020304_05060708L, testObject.readObject(long.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void autoBox_float() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.autoBox_float.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Float|".getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = { (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x04 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final Float expected = Float.intBitsToFloat(0x01020304);
		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(expected, testObject.readObject(float.class));
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void autoBox_double() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.autoBox_double.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Double|".getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = { (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x04, (byte) 0x05, (byte) 0x06,
				(byte) 0x07, (byte) 0x08 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final Double expected = Double.longBitsToDouble(0x01020304_05060708L);
		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(expected, testObject.readObject(double.class));
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void autoBox_boolean() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.autoBox_boolean.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Boolean|".getBytes(StandardCharsets.UTF_8), false);
		FileIoUtil.writeToFile(tempFile, new byte[] { (byte) 0x00 }, true);
		FileIoUtil.writeToFile(tempFile, "java.lang.Boolean|".getBytes(StandardCharsets.UTF_8), true);
		FileIoUtil.writeToFile(tempFile, new byte[] { (byte) 0x01 }, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertFalse(testObject.readObject(boolean.class));
		assertTrue(testObject.readObject(boolean.class));
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void autoBox_char() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.autoBox_char.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Character|".getBytes(StandardCharsets.UTF_8), false);
		FileIoUtil.writeToFile(tempFile, new byte[] { (byte) 0x00, (byte) 0x66 }, true);
		FileIoUtil.writeToFile(tempFile, "java.lang.Character|".getBytes(StandardCharsets.UTF_8), true);
		FileIoUtil.writeToFile(tempFile, new byte[] { (byte) 0x22, (byte) 0x1e }, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals('f', testObject.readObject(char.class).charValue());
		assertEquals('\u221E', testObject.readObject(char.class).charValue());  //infinity sign is BMP non-private
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_overHead_happy() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_overHead_happy.", ".txt");
		tempFile.deleteOnExit();

		//@formatter:off
		final byte[] fileContents = new byte[] {
				(byte)106, (byte)97, (byte)118, (byte)97, (byte)46,  //"java."
				(byte)108, (byte)97, (byte)110, (byte)103, (byte)46,  //"lang."
				(byte)66, (byte)121, (byte)116, (byte)101,  //"Byte"
				(byte)124,  //"|"
				(byte)2  //the data
		};
		//@formatter:on
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(2L, testObject.readObject(Byte.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_overHead_upCast() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_overHead_upCast.", ".txt");
		tempFile.deleteOnExit();

		//@formatter:off
		final byte[] fileContents = new byte[] {
				(byte)106, (byte)97, (byte)118, (byte)97, (byte)46,  //"java."
				(byte)108, (byte)97, (byte)110, (byte)103, (byte)46,  //"lang."
				(byte)66, (byte)121, (byte)116, (byte)101,  //"Byte"
				(byte)124,  //"|"
				(byte)2  //the data
		};
		//@formatter:on
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertEquals(Byte.valueOf((byte) 2), testObject.readObject(Number.class));

		testObject.close();
	}

	@Test
	public void readObject_overHead_boxing() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_overHead_boxing.", ".txt");
		tempFile.deleteOnExit();

		//@formatter:off
		final byte[] fileContents = new byte[] {
				(byte)106, (byte)97, (byte)118, (byte)97, (byte)46,  //"java."
				(byte)108, (byte)97, (byte)110, (byte)103, (byte)46,  //"lang."
				(byte)66, (byte)121, (byte)116, (byte)101,  //"Byte"
				(byte)124,  //"|"
				(byte)2  //the data
		};
		//@formatter:on
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertEquals(Byte.valueOf((byte) 2), testObject.readObject(byte.class));

		testObject.close();
	}

	@Test
	public void readObject_overHead_null() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_overHead_null.", ".txt");
		tempFile.deleteOnExit();

		final byte[] fileContents = new byte[] { (byte) '|' };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertNull(testObject.readObject(Byte.class));
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_overHead_noClassThrows() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_overHead_noClassThrows.", ".txt");
		tempFile.deleteOnExit();

		//@formatter:off
		final byte[] fileContents = new byte[] {
				(byte)106, (byte)97, (byte)118, (byte)97, (byte)46,  //"java."
				(byte)124,  //"|"
				(byte)2  //the data
		};
		//@formatter:on
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);

		try
		{
			testObject.readObject(Object.class);
		}
		catch (final RuntimeException actual)
		{
			assertEquals(ClassNotFoundException.class, actual.getCause().getClass());
		}

		testObject.close();
	}

	@Test
	public void readObject_overHead_noCastThrows() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_overHead_noCastThrows.", ".txt");
		tempFile.deleteOnExit();

		//@formatter:off
		final byte[] fileContents = new byte[] {
				(byte)106, (byte)97, (byte)118, (byte)97, (byte)46,  //"java."
				(byte)108, (byte)97, (byte)110, (byte)103, (byte)46,  //"lang."
				(byte)66, (byte)121, (byte)116, (byte)101,  //"Byte"
				(byte)124,  //"|"
				(byte)2  //the data
		};
		//@formatter:on
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		try
		{
			testObject.readObject(void.class);
			//also covers a possible edge case: void.class.isPrimitive() is true
		}
		catch (final ClassCastException actual)
		{
			assertEquals("java.lang.Byte can't be cast into void", actual.getMessage());
		}

		testObject.close();
	}

	@Test
	public void readObject_overHead_noHeaderThrows() throws IOException
	{
		final File tempFile = File
				.createTempFile("UT_ObjectStreamReader.TempFile.readObject_overHead_noHeaderThrows.", ".txt");
		tempFile.deleteOnExit();
		final byte[] fileContents = { (byte) 2 };
		FileIoUtil.writeToFile(tempFile, fileContents, false);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);

		try
		{
			testObject.readObject(Byte.class);
		}
		catch (final IllegalStateException actual)
		{
			assertEquals("Header not found", actual.getMessage());
		}

		testObject.close();
	}

	@Test
	public void readObject_stops_GenerateId() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_stops_GenerateId.", ".txt");
		tempFile.deleteOnExit();
		final String overhead = "com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$1LocalWithGenerateId|java.lang.String|";
		FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01,  //UTF-8 length (int)
				(byte) 0x66};
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		@GenerateId
		class LocalWithGenerateId {
		}
		final Object data = new LocalWithGenerateId();

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		testObject.getObjectRegistry().registerObject("f", data);

		assertSame(data, testObject.readObject());
		testObject.close();
	}

	@GenerateId
	private final static class ClassWithGenerateIdAndRead implements StaticSerializable {
		public final int data;

		public ClassWithGenerateIdAndRead(final int data) {
			this.data = data;
		}

		public static ClassWithGenerateIdAndRead readFromStream(final ObjectStreamReader reader) {
			final ClassWithGenerateIdAndRead result = new ClassWithGenerateIdAndRead(reader.readObject(int.class));
			reader.getObjectRegistry().claimId(result);
			return result;
		}

		@Override
		public void writeToStream(final ObjectStreamWriter writer) {
			writer.writeObject(data);
		}
	}

	@Test
	public void readObject_continues_GenerateId() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_continues_GenerateId.", ".txt");
		tempFile.deleteOnExit();
		final String overhead = "com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$ClassWithGenerateIdAndRead|java.lang.String|";
		FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01,  //UTF-8 length (int)
				(byte) 0x66};  //id
		FileIoUtil.writeToFile(tempFile, fileContents, true);
		FileIoUtil.writeToFile(tempFile, "java.lang.Integer|".getBytes(StandardCharsets.UTF_8), true);
		FileIoUtil.writeToFile(tempFile, new byte[]{0, 0, 0, 12}, true);  //data to read

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);

		assertEquals(12, testObject.readObject(ClassWithGenerateIdAndRead.class).data);
		assertNotNull(testObject.getObjectRegistry().getRegisteredObject("f"));
		testObject.close();
	}

	@Test
	public void readObject_Byte() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_Byte.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Byte|".getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = { (byte) 2 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(2L, testObject.readObject(Byte.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_Short() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_Short.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Short|".getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = { (byte) 0x0a, (byte) 0xfe };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(0x0afeL, testObject.readObject(Short.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_Integer() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_Integer.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Integer|".getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = { (byte) 0x0a, (byte) 0xfe, (byte) 0xba, (byte) 0xbe };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(0x0afe_babeL, testObject.readObject(Integer.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_Long() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_Long.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Long|".getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = { (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x04, (byte) 0x05, (byte) 0x06,
				(byte) 0x07, (byte) 0x08 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(0x01020304_05060708L, testObject.readObject(Long.class).longValue());
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_Float() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_Float.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Float|".getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = { (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x04 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final Float expected = Float.intBitsToFloat(0x01020304);
		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(expected, testObject.readObject(Float.class));
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_Double() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_Double.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Double|".getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = { (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x04, (byte) 0x05, (byte) 0x06,
				(byte) 0x07, (byte) 0x08 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final Double expected = Double.longBitsToDouble(0x01020304_05060708L);
		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals(expected, testObject.readObject(Double.class));
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_Boolean() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_Boolean.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Boolean|".getBytes(StandardCharsets.UTF_8), false);
		FileIoUtil.writeToFile(tempFile, new byte[] { (byte) 0x00 }, true);
		FileIoUtil.writeToFile(tempFile, "java.lang.Boolean|".getBytes(StandardCharsets.UTF_8), true);
		FileIoUtil.writeToFile(tempFile, new byte[] { (byte) 0x01 }, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertFalse(testObject.readObject(Boolean.class));
		assertTrue(testObject.readObject(Boolean.class));
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_Character() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_Character.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.Character|".getBytes(StandardCharsets.UTF_8), false);
		FileIoUtil.writeToFile(tempFile, new byte[] { (byte) 0x00, (byte) 0x66 }, true);
		FileIoUtil.writeToFile(tempFile, "java.lang.Character|".getBytes(StandardCharsets.UTF_8), true);
		FileIoUtil.writeToFile(tempFile, new byte[] { (byte) 0x22, (byte) 0x1e }, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals('f', testObject.readObject(Character.class).charValue());
		assertEquals('\u221E', testObject.readObject(Character.class).charValue());  //infinity sign is BMP non-private
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_String() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_String.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.lang.String|".getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x04,  //UTF-8 length (int)
				(byte) 0x66, (byte) 0xe2, (byte) 0x88, (byte) 0x9e };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertTrue(testObject.hasData());
		assertEquals("f\u221E", testObject.readObject(String.class));  //infinity sign is BMP (3 UTF-8 bytes) non-private
		assertFalse(testObject.hasData());

		testObject.close();
	}

	private static enum EnumByName implements StaticSerializableEnumByName
	{
		One, Two;
	}

	@Test
	public void readObject_enumByName() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_enumByName.", ".txt");
		tempFile.deleteOnExit();
		final String overhead = "com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$EnumByName|"
				+ "java.lang.String|";
		FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x03 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);
		FileIoUtil.writeToFile(tempFile, "One".getBytes(StandardCharsets.UTF_8), true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertSame(EnumByName.One, testObject.readObject(EnumByName.class));

		testObject.close();
	}

	@Test
	public void readObject_enumByName_nameNotFound() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_enumByName_nameNotFound.", ".txt");
		tempFile.deleteOnExit();
		final String overhead = "com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$EnumByName|"
				+ "java.lang.String|";
		FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x03 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);
		FileIoUtil.writeToFile(tempFile, "six".getBytes(StandardCharsets.UTF_8), true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		try
		{
			testObject.readObject(EnumByName.class);
		}
		catch (final IllegalArgumentException actual)
		{
			assertEquals("No enum constant com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader.EnumByName.six",
					actual.getMessage());
		}

		testObject.close();
	}

	@Test
	public void readObject_enumByName_classNotEnum() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_enumByName_classNotEnum.", ".txt");
		tempFile.deleteOnExit();
		final String overhead = "com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$1NotEnum|"
				+ "java.lang.String|";
		FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x03 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);
		FileIoUtil.writeToFile(tempFile, "One".getBytes(StandardCharsets.UTF_8), true);

		class NotEnum implements StaticSerializableEnumByName
		{}

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		try
		{
			testObject.readObject(NotEnum.class);
		}
		catch (final IllegalArgumentException actual)
		{
			assertEquals("com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$1NotEnum is not an enum type",
					actual.getMessage());
		}

		testObject.close();
	}

	private static enum EnumByOrdinal implements StaticSerializableEnumByOrdinal
	{
		One, Two, Three, Four;
	}

	@Test
	public void readObject_enumByOrdinal() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_enumByOrdinal.", ".txt");
		tempFile.deleteOnExit();
		final String overhead = "com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$EnumByOrdinal|"
				+ "java.lang.Integer|";
		FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x03 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertSame(EnumByOrdinal.Four, testObject.readObject(EnumByOrdinal.class));

		testObject.close();
	}

	@Test
	public void readObject_enumByOrdinal_classNotEnum() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_enumByOrdinal_classNotEnum.",
				".txt");
		tempFile.deleteOnExit();
		final String overhead = "com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$2NotEnum|"
				+ "java.lang.Integer|";
		FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		class NotEnum implements StaticSerializableEnumByOrdinal
		{}

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		try
		{
			testObject.readObject(NotEnum.class);
		}
		catch (final IllegalArgumentException actual)
		{
			assertEquals("com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$2NotEnum"
					+ " implements StaticSerializableEnumByOrdinal but isn't an enum", actual.getMessage());
		}

		testObject.close();
	}

	@Test
	public void readObject_enumByOrdinal_OrdinalNotFound() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_enumByOrdinal_OrdinalNotFound.",
				".txt");
		tempFile.deleteOnExit();
		final String overhead = "com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$EnumByOrdinal|"
				+ "java.lang.Integer|";
		FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x0a };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		try
		{
			testObject.readObject(EnumByOrdinal.class);
		}
		catch (final IllegalStateException actual)
		{
			assertEquals(
					"com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$EnumByOrdinal[10] doesn't exist. Actual length: 4",
					actual.getMessage());
		}

		testObject.close();
	}

	@Test
	public void readObject_custom_happy() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_custom_happy.", ".txt");
		tempFile.deleteOnExit();
		final String overhead = "com.github.SkySpiral7.Java.serialization.testClasses.SimpleHappy|";
		FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8), false);
		FileIoUtil.writeToFile(tempFile, "java.lang.Integer|".getBytes(StandardCharsets.UTF_8), true);
		final byte[] fileContents = new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x04 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		final SimpleHappy readData = testObject.readObject(SimpleHappy.class);
		assertEquals(4, readData.smilyStickersCount);
		assertFalse(testObject.hasData());

		testObject.close();
	}

	@Test
	public void readObject_custom_throw_noReader() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_custom_throw_noReader.", ".txt");
		tempFile.deleteOnExit();
		final String overhead = "com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$1NoReader|";
		FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x04 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		abstract class NoReader implements StaticSerializable
		{}  //abstract and no writer doesn't matter

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		try
		{
			testObject.readObject(NoReader.class);
		}
		catch (final IllegalStateException actual)
		{
			assertEquals("com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$1NoReader"
					+ " implements StaticSerializable but doesn't define readFromStream", actual.getMessage());
		}

		testObject.close();
	}

	private abstract static class NonPublicReader implements StaticSerializable
	{
		@SuppressWarnings("unused")
		protected static NonPublicReader readFromStream(final ObjectStreamReader in)
		{
			return null;
		}
	}

	@Test
	public void readObject_custom_throw_nonPublic() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_custom_throw_nonPublic.", ".txt");
		tempFile.deleteOnExit();
		final String overhead = "com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$NonPublicReader|";
		FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x04 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		try
		{
			testObject.readObject(NonPublicReader.class);
		}
		catch (final IllegalStateException actual)
		{
			assertEquals("com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$NonPublicReader.readFromStream"
					+ " must be public static", actual.getMessage());
		}

		testObject.close();
	}

	@Test
	public void readObject_custom_throw_nonStatic() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_custom_throw_nonStatic.", ".txt");
		tempFile.deleteOnExit();
		final String overhead = "com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$1LocalNonStaticReader|";
		FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x04 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		abstract class LocalNonStaticReader implements StaticSerializable
		{
			@SuppressWarnings("unused")
			public LocalNonStaticReader readFromStream(final ObjectStreamReader in)
			{
				return null;
			}
		}

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		try
		{
			testObject.readObject(LocalNonStaticReader.class);
		}
		catch (final IllegalStateException actual)
		{
			assertEquals("com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$1LocalNonStaticReader.readFromStream"
					+ " must be public static", actual.getMessage());
		}

		testObject.close();
	}

	private abstract static class ThrowingReader implements StaticSerializable
	{
		@SuppressWarnings("unused")
		public static ThrowingReader readFromStream(final ObjectStreamReader in)
		{
			throw new UnsupportedOperationException();
		}
	}

	@Test
	public void readObject_custom_throw_throwingReader() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_custom_throw_throwingReader.",
				".txt");
		tempFile.deleteOnExit();
		final String overhead = "com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$ThrowingReader|";
		FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x04 };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		try
		{
			testObject.readObject(ThrowingReader.class);
		}
		catch (final RuntimeException actual)
		{
			assertEquals("Couldn't deserialize", actual.getMessage());
			assertEquals(InvocationTargetException.class, actual.getCause().getClass());
			assertEquals(UnsupportedOperationException.class, actual.getCause().getCause().getClass());
		}

		testObject.close();
	}

	@Test
	public void readObject_Serializable() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readObject_Serializable.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "java.math.BigInteger|".getBytes(StandardCharsets.UTF_8), false);
		final BigInteger data = BigInteger.TEN;
		final byte[] javaData = ObjectStreamWriter.javaSerialize(data);
		assert(javaData.length < 256);  //currently 203. Possible for the length to change after a Java release
		FileIoUtil.writeToFile(tempFile, new byte[]{0, 0, 0, (byte) javaData.length}, true);
		FileIoUtil.writeToFile(tempFile, javaData, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertEquals(data, testObject.readObject());

		testObject.close();
	}

	private static final class ReflectiveClass implements StaticSerializable
	{
		private int field = 0xdead_beef;

		public static ReflectiveClass readFromStream(final ObjectStreamReader reader)
		{
			final ReflectiveClass result = new ReflectiveClass();
			reader.readFieldsReflectively(result);
			return result;
		}

		@Override
		public void writeToStream(final ObjectStreamWriter writer){}
	}

	@Test
	public void readFieldsReflectively() throws IOException
	{
		final File tempFile = File.createTempFile("UT_ObjectStreamReader.TempFile.readFieldsReflectively.", ".txt");
		tempFile.deleteOnExit();
		final String overhead = "com.github.SkySpiral7.Java.serialization.UT_ObjectStreamReader$ReflectiveClass|java.lang.Integer|";
		FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8), false);
		final byte[] fileContents = { (byte) 0x0a, (byte) 0xfe, (byte) 0xba, (byte) 0xbe };
		FileIoUtil.writeToFile(tempFile, fileContents, true);

		final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
		assertEquals(0x0afe_babeL, testObject.readObject(ReflectiveClass.class).field);

		testObject.close();
	}

}
