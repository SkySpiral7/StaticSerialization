package com.github.SkySpiral7.Java.serialization;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;

import java.io.File;
import java.io.IOException;

import org.junit.Test;

import com.github.SkySpiral7.Java.serialization.testClasses.RootedGraph;
import com.github.SkySpiral7.Java.serialization.testClasses.RootedGraph.Node;
import com.github.SkySpiral7.Java.serialization.testClasses.SimpleHappy;

public class IT_StaticSerializable
{
	@Test
	public void header_notNull() throws IOException
	{
		final File tempFile = File.createTempFile("IT_StaticSerializable.TempFile.header_notNull.", ".txt");
		tempFile.deleteOnExit();
		final String data = "\u221E > \uD83D\uDE22";  //BMP (infinity), ascii, non-BMP (Crying Face)

		final ObjectWriter writer = new ObjectWriter(tempFile);
		writer.writeObject(data);
		writer.close();
		final ObjectReader reader = new ObjectReader(tempFile);
		assertEquals(data, reader.readObject(String.class));
		//don't create tests for each data type. the UT covers those. This is only for making sure the classes agree
		reader.close();
	}

	@Test
	public void header_null() throws IOException
	{
		final File tempFile = File.createTempFile("IT_StaticSerializable.TempFile.header_null.", ".txt");
		tempFile.deleteOnExit();

		final ObjectWriter writer = new ObjectWriter(tempFile);
		writer.writeObject(null);
		writer.close();
		final ObjectReader reader = new ObjectReader(tempFile);
		assertNull(reader.readObject(byte.class));
		reader.close();
	}

	@Test
	public void primitive() throws IOException
	{
		final File tempFile = File.createTempFile("IT_StaticSerializable.TempFile.primitive.", ".txt");
		tempFile.deleteOnExit();
		final byte data = (byte) 2;

		final ObjectWriter writer = new ObjectWriter(tempFile);
		writer.writeObject(data);
		writer.close();
		final ObjectReader reader = new ObjectReader(tempFile);
		assertEquals(Byte.valueOf(data), reader.readObject(byte.class));
		reader.close();
	}

	@Test
	public void custom() throws IOException
	{
		final File tempFile = File.createTempFile("IT_StaticSerializable.TempFile.custom.", ".txt");
		tempFile.deleteOnExit();
		final SimpleHappy data = new SimpleHappy(4);

		final ObjectWriter writer = new ObjectWriter(tempFile);
		writer.writeObject(data);
		writer.close();
		final ObjectReader reader = new ObjectReader(tempFile);

		final SimpleHappy actual = reader.readObject(SimpleHappy.class);
		assertFalse(data == actual);
		assertEquals(data, actual);
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

		final ObjectWriter writer = new ObjectWriter(tempFile);
		writer.writeObject(data);
		writer.close();
		final ObjectReader reader = new ObjectReader(tempFile);

		final RootedGraph actual = (RootedGraph) reader.readObject();
		assertFalse(data == actual);
		assertEquals(data, actual);
		reader.close();
	}

}
