package com.github.SkySpiral7.Java.serialization;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

import java.lang.reflect.Field;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import com.github.SkySpiral7.Java.util.ClassUtil;

public class UT_ClassUtil
{
	@Test
	public <T extends Enum<T>> void cast_compiles()
	{
		final Class<?> questionable = RoundingMode.class;

		@SuppressWarnings("unchecked")
		final Class<T> unchecked = ((Class<T>) questionable);
		//not convenient and only possible because of the method's capturing

		assertThat(Enum.valueOf(unchecked, "UP"), is(RoundingMode.UP));
		assertThat(Enum.valueOf(ClassUtil.cast(questionable), "UP"), is(RoundingMode.UP));
	}

	@Test
	public void getAllFields() throws Exception
	{
		class ClassA
		{
			@SuppressWarnings("unused")
			public int fieldA = 1;
		}
		class ClassB extends ClassA
		{
			@SuppressWarnings("unused")
			public int fieldB = 2;
		}
		//by virtue of being non-static these local classes contain a generated field (this$0) which references UT_ClassUtil

		final List<Field> expected = new ArrayList<>();
		expected.add(ClassB.class.getField("fieldB"));
		expected.add(ClassA.class.getField("fieldA"));

		assertThat(ClassUtil.getAllFields(ClassB.class), is(expected));
	}

}
