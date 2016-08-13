package com.github.SkySpiral7.Java.serialization;

import java.util.HashMap;
import java.util.Map;

public class ObjectRegistry
{
	private final Map<Key, Object> registry = new HashMap<>();

	public void registerObject(final Object id, final Object instance){}
	public <T> T getRegisteredObject(final Object id, final Class<T> classOfInstance){return null;}

	private static class Key{}
}
