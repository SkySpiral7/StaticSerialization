package com.github.SkySpiral7.Java.serialization;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.List;
import java.util.stream.Collectors;

import com.github.SkySpiral7.Java.util.ClassUtil;

public enum SerializationUtil
{
	;  //no instances

	public static List<Field> getAllSerializableFields(final Class<?> subject)
	{
		final List<Field> allFields = ClassUtil.getAllFields(subject);
		return allFields.stream().filter(field -> {
			final int modifiers = field.getModifiers();
			if (Modifier.isFinal(modifiers)) return false;  //can't be read from stream
				if (Modifier.isTransient(modifiers)) return false;  //shouldn't be touched
				if (Modifier.isStatic(modifiers)) return false;  //not related to the instance

				final Class<?> type = field.getType();
				//known hole: doesn't support T extends StaticSerializable
				if (type.equals(void.class)) return false;  //I don't think this is possible...
				if (type.isPrimitive()) return true;
				if (ClassUtil.isBoxedPrimitive(type)) return true;
				if (type.equals(String.class)) return true;
				if (StaticSerializable.class.isAssignableFrom(type)) return true;
				//if (Serializable.class.isAssignableFrom(type)) return true;

				return false;
			}).collect(Collectors.toList());
	}
}
