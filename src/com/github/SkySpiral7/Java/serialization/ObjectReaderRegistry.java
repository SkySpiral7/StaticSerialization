package com.github.SkySpiral7.Java.serialization;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import com.github.SkySpiral7.Java.util.ClassUtil;

public class ObjectReaderRegistry
{
	private final Map<String, Object> registry = new HashMap<>();

	public void registerObject(final String id, final Object instance)
	{
		Objects.requireNonNull(id);
		Objects.requireNonNull(instance);
		registry.put(id, instance);
	}

	public <T> T getRegisteredObject(final String id)
	{
		Objects.requireNonNull(id);
		return ClassUtil.cast(registry.get(id));
	}

}
