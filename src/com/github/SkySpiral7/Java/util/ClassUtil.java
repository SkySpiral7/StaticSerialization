package com.github.SkySpiral7.Java.util;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public enum ClassUtil
{
	;  //no instances

	/**
	 * <p>
	 * This method will do an unchecked cast to T (the captured return type). The idea is that calling this method allows
	 * you to avoid raw types and unchecked casting warnings. Since T will be erased, calling this method won't throw. If
	 * the return type of this method is ambiguous, it may take the first choice instead of the desired class. And since
	 * overloading is determined at compile time you should double check that you got what you intended.
	 * </p>
	 * <p>
	 * If you have a {@code Class<T> clazz} then instead call clazz.cast which is unchecked and might throw (which is
	 * good). clazz.cast also avoids ambiguities mentioned above.
	 * </p>
	 * 
	 * @param anything
	 *           will cast this to T
	 * @return anything as T
	 */
	@SuppressWarnings("unchecked")
	public static <T> T cast(final Object anything)
	{
		return (T) anything;
	}

	/**
	 * This method returns the List of all Fields for a class.
	 * All fields are included regardless of scope (eg private, public),
	 * and regardless of modifiers (eg static, transient).
	 * All fields are included in the base class and all parent class,
	 * however enclosing classes are excluding because such Fields can't be used directly on an object of class anyClass.
	 * Additionally generated fields are excluded because you usually don't want to touch them.
	 * 
	 * @return a List of every possible Field that can be used by any object of the given anyClass
	 *         except generated ones
	 */
	public static List<Field> getAllFields(final Class<?> anyClass)
	{
		final List<Class<?>> allClasses = new ArrayList<>();
		Class<?> cursor = anyClass;
		while (cursor != null)
		{
			allClasses.add(cursor);
			cursor = cursor.getSuperclass();
		}

		final List<Field> result = new ArrayList<>();
		allClasses.stream().forEach(clazz -> {
			result.addAll(Arrays.asList(clazz.getDeclaredFields()));
		});
		return result.stream().filter(field -> {
			//exclude generated fields
				return !field.getName().contains("$");
			}).collect(Collectors.toList());
	}

	/**
	 * @return true if an instance of classInQuestion could be auto-unboxed.
	 * @see Class#isPrimitive()
	 */
	public static boolean isBoxedPrimitive(final Class<?> classInQuestion)
	{
		return Arrays.asList(Byte.class, Short.class, Integer.class, Long.class,  //integers
				Float.class, Double.class, Boolean.class, Character.class).contains(classInQuestion);
	}
}
