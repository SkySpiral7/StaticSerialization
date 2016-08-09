package com.github.SkySpiral7.Java.pojo;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;

import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Factory;
import org.hamcrest.Matcher;

/**
 * <p>
 * This matcher determines if the actual value can be serialized regardless of what data it has a the time of
 * serialization (assuming that type erasure is not abused). This matcher checks for arrays, primitives, inner classes,
 * wild card bounds, enums, generic fields, and parameterized types of classes.
 * </p>
 * <p>
 * This matcher will only check classes whose fully qualified name starts with packageToInspect so that the caller may
 * evaluate a class in any package without needlessly inspecting java.* etc. For example we can trust
 * java.util.ArrayList is Serializable because it implements Serializable and we can assume the JRE is free of
 * serialization bugs. Although not recommended you may pass in the empty string in order to inspect all classes.
 * </p>
 *
 * @see #isDeeplySerializable(String)
 * @see #isDeeplySerializable(String, Set)
 * @see #ASSUMED_SERIALIZABLE_DEFAULTS
 */
public final class DeepSerializableMatcher extends BaseMatcher<Class<? extends Serializable>>
{

	private final String packageToInspect;
	private final Set<Class<?>> assumedSerializable;
	private Collection<String> allNonSerializableClassHistories = null;
	private final SerializableInspector inspector = new SerializableInspector();

	/**
	 * @see #isDeeplySerializable(String)
	 * @see #isDeeplySerializable(String, Set)
	 */
	private DeepSerializableMatcher(String packageToInspect, final Set<Class<?>> assumedSerializable)
	{
		if (!packageToInspect.isEmpty() && !packageToInspect.endsWith(".")) packageToInspect += ".";
		//this prevents matching partial package names. Empty is allowed (but not recommended)
		this.packageToInspect = packageToInspect;
		this.assumedSerializable = new HashSet<>(assumedSerializable);  //defensive copy
	}

	@Override
	public boolean matches(final Object item)
	{
		if (!(item instanceof Class)) throw new IllegalStateException("detected abuse of type erasure");
		allNonSerializableClassHistories = inspector.findAllNonSerializableClasses((Class<?>) item);
		return allNonSerializableClassHistories.isEmpty();
	}

	@Override
	public void describeTo(final Description description)
	{
		if (null == allNonSerializableClassHistories) throw new IllegalStateException("must call matches first");
		description.appendText("a Serializable class");
	}

	@Override
	public void describeMismatch(final Object item, final Description description)
	{
		if (null == allNonSerializableClassHistories) throw new IllegalStateException("must call matches first");
		description.appendText("found these classes:\n");
		for (final String singleClassHistory : allNonSerializableClassHistories)
		{
			description.appendText("\t");
			description.appendText(singleClassHistory);
			description.appendText("\n");
		}
	}

	/**
	 * <p>
	 * Creates a matcher that matches if the examined class is deeply serializable. This method uses the value of
	 * ASSUMED_SERIALIZABLE_DEFAULTS to create the matcher.
	 * </p>
	 * <p>
	 * For example:
	 * 
	 * <pre>
	 * {@code assertThat(MyClass.class, isDeeplySerializable("com.base.package."))}
	 * </pre>
	 * 
	 * </p>
	 *
	 * @param packageToInspect
	 *           the string of the untrusted package. It should end with a dot such as "org.apache.commons.lang."
	 * @see DeepSerializableMatcher
	 * @see #ASSUMED_SERIALIZABLE_DEFAULTS
	 * @see #isDeeplySerializable(String, Set)
	 */
	@Factory
	public static Matcher<Class<? extends Serializable>> isDeeplySerializable(final String packageToInspect)
	{
		return new DeepSerializableMatcher(packageToInspect, ASSUMED_SERIALIZABLE_DEFAULTS);
	}

	/**
	 * <p>
	 * Creates a matcher that matches if the examined class is deeply serializable.
	 * </p>
	 * <p>
	 * For example:
	 * 
	 * <pre>
	 * {@code Set<Class<?>> assumedSerializable = new HashSet<>(Arrays.asList(List.class, Set.class));
	 * assertThat(MyClass.class, isDeeplySerializable("com.base.package.", assumedSerializable))}
	 * </pre>
	 * 
	 * </p>
	 *
	 * @param packageToInspect
	 *           the string of the untrusted package. It should end with a dot such as "org.apache.commons.lang."
	 * @param assumedSerializable
	 *           the set of classes to use instead of the default value (see ASSUMED_SERIALIZABLE_DEFAULTS for
	 *           a description of what it does).
	 * @see DeepSerializableMatcher
	 * @see #ASSUMED_SERIALIZABLE_DEFAULTS
	 * @see #isDeeplySerializable(String)
	 */
	@Factory
	public static Matcher<Class<? extends Serializable>> isDeeplySerializable(final String packageToInspect,
			final Set<Class<?>> assumedSerializable)
	{
		return new DeepSerializableMatcher(packageToInspect, assumedSerializable);
	}

	/**
	 * <p>
	 * This matcher will assume that the implementation of these classes will be Serializable. Note that HashSet,
	 * HashMap, and ArrayList are all Serializable. Since this is intended to be used by unit tests this set is in no way
	 * synchronized.
	 * </p>
	 * <p>
	 * This set is used as the default value by isDeeplySerializable(String) and a defensive copy is made.
	 * </p>
	 * 
	 * @see #isDeeplySerializable(String)
	 * @see #isDeeplySerializable(String, Set)
	 */
	public static final Set<Class<?>> ASSUMED_SERIALIZABLE_DEFAULTS = new HashSet<>(Arrays.asList(Collection.class,
			Set.class, List.class, Map.class));

	/**
	 * This class exists only for logical separation. DeepSerializableMatcher contains everything related to being a
	 * matcher
	 * and Inspector contains all of the logic for inspecting the given class.
	 */
	private final class SerializableInspector
	{
		/**
		 * @param startingClass
		 *           the class that will be scanned for non-Serializable fields or generics.
		 * @return each string returned contains a readable description of where the non-Serializable type is located.
		 *         Note that the same class may have multiple fields of the same non-Serializable which will appear each
		 *         time
		 */
		public Collection<String> findAllNonSerializableClasses(final Class<?> startingClass)
		{
			final Set<Class<?>> serializableClasses = new HashSet<>();
			final Deque<ClassHistory> uncheckedClassLocations = new ArrayDeque<>();
			final List<String> nonSerializableClassLocations = new ArrayList<>();

			newClassEvent(uncheckedClassLocations, startingClass.getName() + " ", startingClass);
			while (!uncheckedClassLocations.isEmpty())
			{
				final ClassHistory classHistory = uncheckedClassLocations.removeLast();
				final Class<?> classToCheck = classHistory.getCurrent();
				if (serializableClasses.contains(classToCheck)) continue;  //this prevents getting stuck in this loop
				//additionally because non-serializable classes aren't skipped they will fail for each location they are referenced
				if (!Serializable.class.isAssignableFrom(classToCheck)) nonSerializableClassLocations.add(classHistory
						.getHistory().append("which is not Serializable").toString());
				else
				{
					serializableClasses.add(classToCheck);

					if (classToCheck.getName().startsWith(packageToInspect)) collectClassPieces(uncheckedClassLocations,
							classHistory, classToCheck);
				}
			}

			return nonSerializableClassLocations;
		}

		/**
		 * This method adds a new ClassHistory to uncheckedClassLocations for each field of classToCheck, the super class
		 * of classToCheck,
		 * and the containing class of classToCheck (if classToCheck is an inner class).
		 *
		 * @param uncheckedClassLocations
		 *           a collection of every class (in a ClassHistory) that has not yet been examined
		 * @param classHistory
		 *           the ClassHistory for the class currently being examined
		 * @param classToCheck
		 *           the class contained by the current classHistory
		 */
		private void collectClassPieces(final Deque<ClassHistory> uncheckedClassLocations,
				final ClassHistory classHistory, final Class<?> classToCheck)
		{
			for (final Field field : getAllFields(classToCheck))
			{
				newEvents(uncheckedClassLocations, classHistory.getHistory().append("contains field ").toString(),
						getGenerics(field.getGenericType()));
			}
			final Class<?> superClass = classToCheck.getSuperclass();
			//don't need to check interfaces since they can only contain static fields
			if (!Object.class.equals(superClass))
			{
				final String containedInHistory = classHistory.getHistory().append("is a child of class ")
						.append(superClass.getName()).append(' ').toString();
				newClassEvent(uncheckedClassLocations, containedInHistory, superClass);
			}
			//inner classes don't affect serialization
			//however the parents of a non-static inner class do
			final Class<?> declaringClass = classToCheck.getDeclaringClass();
			if (null != declaringClass && !Modifier.isStatic(classToCheck.getModifiers()))
			{
				final String containedInHistory = classHistory.getHistory().append("is contained in class ")
						.append(declaringClass.getName()).append(' ').toString();
				newClassEvent(uncheckedClassLocations, containedInHistory, declaringClass);
			}
			//ask to confirm that getEnclosingClass doesn't apply to me
		}

		/**
		 * This method creates a new ClassHistory from newClassEvent and adds it and all of the class generics to
		 * uncheckedClassLocations.
		 *
		 * @param uncheckedClassLocations
		 *           the collection of ClassHistorys that have not been checked for being serializable.
		 * @param pastHistory
		 *           provided by the current class's ClassHistory
		 * @param newClassEvent
		 *           a class that was discovered by the current class
		 * @see #getClassGenerics(Class)
		 */
		private void newClassEvent(final Deque<ClassHistory> uncheckedClassLocations, final String pastHistory,
				final Class<?> newClassEvent)
		{
			final ClassHistory item = new ClassHistory(pastHistory, newClassEvent);
			uncheckedClassLocations.addLast(item);

			final StringBuilder initialHistory = new StringBuilder(pastHistory);
			initialHistory.append("is parameterized with ");
			newEvents(uncheckedClassLocations, initialHistory.toString(), getClassGenerics(newClassEvent));
		}

		/**
		 * For each element of newEvents a new ClassHistory is created and added to uncheckedClassLocations.
		 *
		 * @param uncheckedClassLocations
		 *           the collection of ClassHistorys that have not been checked for being serializable.
		 * @param pastHistory
		 *           provided by the current class's ClassHistory. All elements in newEvents have the same history.
		 * @param newEvents
		 *           a list of classes that need to be checked for serializable. Such as a list of fields in a class.
		 */
		private void newEvents(final Deque<ClassHistory> uncheckedClassLocations, final String pastHistory,
				final List<Class<?>> newEvents)
		{
			for (final Class<?> event : newEvents)
			{
				final ClassHistory item = new ClassHistory(new StringBuilder(pastHistory).append(event.getName())
						.append(' ').toString(), event);
				uncheckedClassLocations.addLast(item);
			}
		}

		/**
		 * @return a list of all fields (including private ones) of startingClass excluding static and transient.
		 */
		private List<Field> getAllFields(final Class<?> startingClass)
		{
			final List<Field> allFields = new ArrayList<>();
			for (final Field field : startingClass.getDeclaredFields())
			{
				//static and transient don't affect serialization
				//ignore synthetic fields since I'll check the declaringClass anyway
				if (!Modifier.isStatic(field.getModifiers()) && !Modifier.isTransient(field.getModifiers())
						&& !field.isSynthetic()) allFields.add(field);
			}
			return allFields;
		}

		/**
		 * Given any class (clazz) this method sends each parameterized type to getGenerics
		 * and returns the resulting list.
		 * For example {@code class Map<K, V>} would send {@code [K, V]} to getGenerics.
		 * 
		 * @see #getGenerics(Type)
		 */
		private List<Class<?>> getClassGenerics(final Class<?> clazz)
		{
			//TODO: currently only allows class's <T> to be serializable even if T is isn't a field or is transient
			//TODO: currently does not allow <T extends Serializable & Cloneable>
			final List<Class<?>> genericClassList = new ArrayList<>();
			for (final TypeVariable<?> typeVariable : clazz.getTypeParameters())
			{
				for (final Type bound : typeVariable.getBounds())
				{
					//each bound can only be a Class or ParameterizedType
					genericClassList.addAll(getGenerics(bound));
				}
			}
			return genericClassList;
		}

		/**
		 * Given any type (startingType) this method deeply searches the parameterized types,
		 * collects them, sends them to filterClasses, then returns the resulting list.
		 * For example {@code List<Map<Short, Comparable<Double>>>} becomes {@code [List, Map, Short, Comparable, Double]}
		 * before being sent to filterClasses.
		 * 
		 * @see #filterClasses(List)
		 */
		private List<Class<?>> getGenerics(final Type startingType)
		{
			final List<Class<?>> genericClassList = new ArrayList<>();
			final Deque<Type> uncheckedTypes = new ArrayDeque<>();
			uncheckedTypes.addLast(startingType);
			while (!uncheckedTypes.isEmpty())
			{
				final Type genericType = uncheckedTypes.removeLast();
				if (genericType instanceof Class)
				{
					//if it isn't generic then just add it
					genericClassList.add((Class<?>) genericType);
				}
				else if (genericType instanceof GenericArrayType)
				{
					//example: public List<String>[] f1;
					final GenericArrayType genericArrayType = (GenericArrayType) genericType;
					uncheckedTypes.addLast(genericArrayType.getGenericComponentType());
				}
				else if (genericType instanceof ParameterizedType)
				{
					final ParameterizedType parameterizedType = (ParameterizedType) genericType;
					uncheckedTypes.addLast(parameterizedType.getRawType());
					uncheckedTypes.addAll(Arrays.asList(parameterizedType.getActualTypeArguments()));
				}
				else if (genericType instanceof TypeVariable)
				{
					//example: public E[] elements; such as from List<E> ignore this since we would have already checked the field List<String>
					//ignoring this also stops Enum<T extends Enum<T>> from being infinite
				}
				else if (genericType instanceof WildcardType)
				{
					final WildcardType wildcardType = (WildcardType) genericType;
					//upper bounds example: public List<? extends Serializable>[] f6;
					//lower bounds would be <? super List<?>> which doesn't affect serialization
					uncheckedTypes.addAll(Arrays.asList(wildcardType.getUpperBounds()));
					//ask to confirm that I'm not missing something about Wild cards
					//specifically: when will this ever return more than 1?
				}
			}

			return filterClasses(genericClassList);
		}

		/**
		 * This method converts all array classes of genericClassList into their base classes,
		 * removes primitives and enums, and removes all classes that are contained in ASSUMED_SERIALIZABLE.
		 * 
		 * @see #ASSUMED_SERIALIZABLE_DEFAULTS
		 */
		private List<Class<?>> filterClasses(final List<Class<?>> genericClassList)
		{
			ListIterator<Class<?>> genericIterator = genericClassList.listIterator();
			while (genericIterator.hasNext())
			{
				final Class<?> lookAt = genericIterator.next();
				//arrays are implicitly Serializable but the component type still needs to be checked
				if (lookAt.isArray()) genericIterator.set(getArrayBaseComponent(lookAt));
			}
			//must take 2 passes so that int[] is filtered out in the second pass
			genericIterator = genericClassList.listIterator();
			while (genericIterator.hasNext())
			{
				final Class<?> lookAt = genericIterator.next();
				//primitives and enums are implicitly Serializable and not generic. So they don't need to be checked
				if (lookAt.isPrimitive() || lookAt.isEnum()) genericIterator.remove();
				else if (assumedSerializable.contains(lookAt)) genericIterator.remove();
			}

			return genericClassList;
		}

		/**
		 * This method converts an array to its base component. For example an int[][][] would become an int.
		 */
		private Class<?> getArrayBaseComponent(Class<?> arrayClass)
		{
			while (arrayClass.isArray())
				arrayClass = arrayClass.getComponentType();
			return arrayClass;
		}
	}

	/**
	 * This class exists in order to track both the class that needs to be examined and the description of where it is
	 * located.
	 * This class doesn't exist in Inspector so that it can be static.
	 */
	private static final class ClassHistory
	{
		private final String history;
		private final Class<?> current;

		/**
		 * @param history
		 *           a string description of where current is located.
		 * @param current
		 *           the class that has yet to be examined.
		 */
		public ClassHistory(final String history, final Class<?> current)
		{
			this.history = history;
			this.current = current;
		}

		/**
		 * @return a copy of the history. Mutating the returned value will not affect this ClassHistory
		 */
		public StringBuilder getHistory()
		{
			return new StringBuilder(history);
		}

		/**
		 * @return the class that has yet to be examined.
		 */
		public Class<?> getCurrent()
		{
			return current;
		}
	}
}
