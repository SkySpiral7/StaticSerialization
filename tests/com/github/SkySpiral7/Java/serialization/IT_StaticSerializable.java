package com.github.SkySpiral7.Java.serialization;

import com.github.SkySpiral7.Java.serialization.testClasses.RootedGraph;
import com.github.SkySpiral7.Java.serialization.testClasses.RootedGraph.Node;
import com.github.SkySpiral7.Java.serialization.testClasses.SimpleHappy;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;

import static org.junit.Assert.*;

public class IT_StaticSerializable
{
	@Test
	public void header_notNull() throws IOException
	{
		final File tempFile = File.createTempFile("IT_StaticSerializable.TempFile.header_notNull.", ".txt");
		tempFile.deleteOnExit();
		final String data = "\u221E > \uD83D\uDE22";  //BMP (infinity), ascii, non-BMP (Crying Face)

		final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
		writer.writeObject(data);
		writer.close();
		final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
		assertEquals(data, reader.readObject(String.class));
		//don't create tests for each data type. the UT covers those. This is only for making sure the classes agree
		reader.close();
	}

	@Test
	public void header_null() throws IOException
	{
		final File tempFile = File.createTempFile("IT_StaticSerializable.TempFile.header_null.", ".txt");
		tempFile.deleteOnExit();

		final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
		writer.writeObject(null);
		writer.close();
		final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
		assertNull(reader.readObject(byte.class));
		reader.close();
	}

	@Test
	public void primitive() throws IOException
	{
		final File tempFile = File.createTempFile("IT_StaticSerializable.TempFile.primitive.", ".txt");
		tempFile.deleteOnExit();
		final byte data = (byte) 2;

		final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
		writer.writeObject(data);
		writer.close();
		final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
		assertEquals(Byte.valueOf(data), reader.readObject(byte.class));
		reader.close();
	}

	@Test
	public void custom() throws IOException
	{
		final File tempFile = File.createTempFile("IT_StaticSerializable.TempFile.custom.", ".txt");
		tempFile.deleteOnExit();
		final SimpleHappy data = new SimpleHappy(4);

		final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
		writer.writeObject(data);
		writer.close();
		final ObjectStreamReader reader = new ObjectStreamReader(tempFile);

		final SimpleHappy actual = reader.readObject(SimpleHappy.class);
		assertNotSame(data, actual);
		assertEquals(data, actual);
		reader.close();
	}

	private static enum EnumByName implements StaticSerializableEnumByName
	{
		One, Two;
	}

	@Test
	public void enumByName() throws IOException
	{
		final File tempFile = File.createTempFile("IT_StaticSerializable.TempFile.enumByName.", ".txt");
		tempFile.deleteOnExit();

		final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
		writer.writeObject(EnumByName.One);
		writer.close();
		final ObjectStreamReader reader = new ObjectStreamReader(tempFile);

		assertSame(EnumByName.One, reader.readObject(EnumByName.class));
		reader.close();
	}

	private static enum EnumByOrdinal implements StaticSerializableEnumByOrdinal
	{
		One, Two, Three, Four;
	}

	@Test
	public void enumByOrdinal() throws IOException
	{
		final File tempFile = File.createTempFile("IT_StaticSerializable.TempFile.enumByOrdinal.", ".txt");
		tempFile.deleteOnExit();

		final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
		writer.writeObject(EnumByOrdinal.Four);
		writer.close();
		final ObjectStreamReader reader = new ObjectStreamReader(tempFile);

		assertSame(EnumByOrdinal.Four, reader.readObject(EnumByOrdinal.class));
		reader.close();
	}

	@Test
	public void getObjectRegistry() throws IOException
	{
		final File tempFile = File.createTempFile("IT_StaticSerializable.TempFile.getObjectRegistry.", ".txt");
		tempFile.deleteOnExit();
		final RootedGraph data;
		{
			final Node alice = new Node("Alice");
			final Node bob = new Node("Bob");
			final Node clark = new Node("Clark");

			alice.links.add(bob);
			bob.links.add(clark);
			clark.links.add(bob);
			clark.links.add(clark);
			//a -> b <-> c -> c

			data = new RootedGraph(alice);
		}

		final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
		writer.writeObject(data);
		writer.close();
		final ObjectStreamReader reader = new ObjectStreamReader(tempFile);

		final RootedGraph actual = (RootedGraph) reader.readObject();
		assertNotSame(data, actual);
		assertEquals(data, actual);
		reader.close();
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
		public void writeToStream(final ObjectStreamWriter writer)
		{
			writer.writeFieldsReflectively(this);
		}
	}

	@Test
	public void reflection() throws IOException
	{
		final File tempFile = File.createTempFile("IT_StaticSerializable.TempFile.reflection.", ".txt");
		tempFile.deleteOnExit();
		final ReflectiveClass data = new ReflectiveClass();
		data.field = 0x0afe_babe;

		final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
		writer.writeObject(data);
		writer.close();
		final ObjectStreamReader reader = new ObjectStreamReader(tempFile);

		final ReflectiveClass actual = reader.readObject(ReflectiveClass.class);
		assertNotSame(data, actual);
		assertEquals(data.field, actual.field);
		reader.close();
	}

	@Test
	public void serializable() throws IOException
	{
		final File tempFile = File.createTempFile("IT_StaticSerializable.TempFile.serializable.", ".txt");
		tempFile.deleteOnExit();
		final BigInteger data = BigInteger.TEN;

		final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
		writer.writeObject(data);
		writer.close();
		final ObjectStreamReader reader = new ObjectStreamReader(tempFile);

		final BigInteger actual = (BigInteger) reader.readObject();
		assertNotSame(data, actual);  //TEN is not a singleton and BigInteger won't readResolve it to be the same
		assertEquals(data, actual);
		reader.close();
	}
}
