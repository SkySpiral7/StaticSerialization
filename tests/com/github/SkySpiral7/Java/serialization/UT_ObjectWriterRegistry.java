package com.github.SkySpiral7.Java.serialization;

import static org.junit.Assert.assertSame;

import java.util.UUID;

import org.junit.Before;
import org.junit.Test;

public class UT_ObjectWriterRegistry
{
	private ObjectWriterRegistry testObject;

	@Before
	public void setUp()
	{
		testObject = new ObjectWriterRegistry();
	}

	@Test(expected = NullPointerException.class)
	public void registerObject_nullId()
	{
		testObject.registerObject(null, "");
	}

	@Test(expected = NullPointerException.class)
	public void registerObject_nullValue()
	{
		testObject.registerObject("", null);
	}

	@Test(expected = NullPointerException.class)
	public void registerObject_object_nullValue()
	{
		testObject.registerObject(null);
	}

	@Test(expected = NullPointerException.class)
	public void getId_nullValue()
	{
		testObject.getId(null);
	}

	@Test
	public void getId_happyAuto()
	{
		final String data = "test me";
		final String id = testObject.registerObject(data);
		assertSame(id, testObject.getId(data));
	}

	@Test
	public void getId_happyManual()
	{
		final String data = "test me";
		final String id = UUID.randomUUID().toString();

		testObject.registerObject(id, data);
		assertSame(id, testObject.getId(data));
	}

}
