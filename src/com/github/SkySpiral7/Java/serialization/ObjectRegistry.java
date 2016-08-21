package com.github.SkySpiral7.Java.serialization;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import com.github.SkySpiral7.Java.util.ClassUtil;

public class ObjectRegistry
{
	private final Map<Key, Object> registry = new HashMap<>();

	public void registerObject(final Object id, final Object instance)
	{
		Objects.requireNonNull(id);
		//instance throws NPE on the next line:
		registry.put(new Key(id, instance.getClass()), instance);
	}

	public <T> T getRegisteredObject(final Object id, final Class<T> classOfInstance)
	{
		Objects.requireNonNull(id);
		Objects.requireNonNull(classOfInstance);
		return ClassUtil.cast(registry.get(new Key(id, classOfInstance)));
	}

	private static final class Key
	{
		private final Object id;
		private final Class<?> classOfInstance;

		public Key(final Object id, final Class<?> classOfInstance)
		{
			this.id = id;
			this.classOfInstance = classOfInstance;
		}

		@Override
		public boolean equals(final Object obj)
		{
			if (!(obj instanceof Key)) return false;
			final Key other = (Key) obj;
			return (this.id.equals(other.id) && this.classOfInstance.equals(other.classOfInstance));
		}

		@Override
		public int hashCode()
		{
			return (id.hashCode() ^ classOfInstance.hashCode());
		}

	}
}
