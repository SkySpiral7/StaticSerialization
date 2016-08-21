package com.github.SkySpiral7.Java.serialization;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.UUID;

import org.junit.Before;
import org.junit.Test;

public class UT_ObjectReaderRegistry
{
	private ObjectReaderRegistry testObject;

	@Before
	public void setUp()
	{
		testObject = new ObjectReaderRegistry();
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
	public void getRegisteredObject_nullId()
	{
		testObject.getRegisteredObject(null);
	}

	@Test
	public void getRegisteredObject_happy()
	{
		final String id = UUID.randomUUID().toString();
		final String expected = "test me";

		testObject.registerObject(id, expected);
		assertEquals(expected, testObject.getRegisteredObject(id));
	}

	@Test
	public void getRegisteredObject_idNotFound()
	{
		testObject.registerObject(UUID.randomUUID().toString(), "test me");
		assertNull(testObject.getRegisteredObject(UUID.randomUUID().toString()));
	}

}
