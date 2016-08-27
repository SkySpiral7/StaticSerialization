package com.github.SkySpiral7.Java.serialization;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.List;
import java.util.stream.Collectors;

import com.github.SkySpiral7.Java.util.ClassUtil;

/**
 * This utility class is for helping StaticSerializable, not for Serialization in general which is
 * why it isn't in the general util package.
 */
public enum SerializationUtil
{
	;  //no instances

    /**
     * @param subject the class to inspect
     * @return all fields that can and should be written with ObjectWriter.
     * Known limitation: doesn't support fields of TypeVariable that extend StaticSerializable.
     * @see ClassUtil#getAllFields(Class)
     */
    public static List<Field> getAllSerializableFields(final Class<?> subject)
	{
		final List<Field> allFields = ClassUtil.getAllFields(subject);
		return allFields.stream().filter(field -> {
            final int modifiers = field.getModifiers();
            if (Modifier.isFinal(modifiers)) return false;  //can't be read from stream
            if (Modifier.isTransient(modifiers)) return false;  //shouldn't be touched
            if (Modifier.isStatic(modifiers)) return false;  //not related to the instance

            final Class<?> type = field.getType();
            if (type.isPrimitive()) return true;  //pretty sure type.equals(void.class) isn't possible
            if (ClassUtil.isBoxedPrimitive(type)) return true;
            if (type.equals(String.class)) return true;
            if (StaticSerializable.class.isAssignableFrom(type)) return true;
            //if (Serializable.class.isAssignableFrom(type)) return true;

            return false;
        }).collect(Collectors.toList());
	}
}
