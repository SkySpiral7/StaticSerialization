package com.github.SkySpiral7.Java.serialization;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.UUID;

import org.junit.Before;
import org.junit.Test;

public class UT_ObjectRegistry
{
	private ObjectRegistry testObject;

	@Before
	public void setUp()
	{
		testObject = new ObjectRegistry();
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
		testObject.getRegisteredObject(null, String.class);
	}

	@Test(expected = NullPointerException.class)
	public void getRegisteredObject_nullValue()
	{
		testObject.getRegisteredObject("", null);
	}

	@Test
	public void getRegisteredObject_happy()
	{
		final UUID id = UUID.randomUUID();
		final String expected = "test me";

		testObject.registerObject(id, expected);
		assertEquals(expected, testObject.getRegisteredObject(id, String.class));
	}

	@Test
	public void getRegisteredObject_idNotFound()
	{
		testObject.registerObject(UUID.randomUUID(), "test me");
		assertNull(testObject.getRegisteredObject(UUID.randomUUID(), String.class));
	}

	@Test
	public void getRegisteredObject_classNotFound()
	{
		final String id = "id thing";
		testObject.registerObject(id, 12);
		assertNull(testObject.getRegisteredObject(id, Number.class));
	}
}
