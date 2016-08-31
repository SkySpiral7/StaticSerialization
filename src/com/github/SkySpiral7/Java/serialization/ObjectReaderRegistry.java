package com.github.SkySpiral7.Java.serialization;

import com.github.SkySpiral7.Java.util.ClassUtil;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

public class ObjectReaderRegistry
{
	private final Map<String, Object> registry = new HashMap<>();
	private String unclaimedId;

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

	public <T> T readObjectOrId(final ObjectStreamReader reader)
	{
		Objects.requireNonNull(reader);
		final String id = reader.readObject(String.class);
		if(registry.containsKey(id)) return ClassUtil.cast(registry.get(id));
		unclaimedId = id;
		return ClassUtil.cast(reader.readObject());
	}

	public void claimId(final Object instance)
	{
		Objects.requireNonNull(instance);
		//TODO: if root element will throw NPE
		registry.put(unclaimedId, instance);
		unclaimedId = null;
	}
}
