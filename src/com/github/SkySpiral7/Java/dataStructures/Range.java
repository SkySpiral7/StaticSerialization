package com.github.SkySpiral7.Java.dataStructures;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import com.github.SkySpiral7.Java.pojo.Comparison;
import com.github.SkySpiral7.Java.util.ComparableSugar;

public final class Range<T extends Number & Comparable<T>>
{
	private final Boundary<T> lower;
	private final Boundary<T> upper;

	public Range(final Boundary<T> lower, final Boundary<T> upper)
	{
		Objects.requireNonNull(lower, "lower");
		Objects.requireNonNull(upper, "upper");
		if(ComparableSugar.is(lower.getValue(), Comparison.GREATER_THAN_OR_EQUAL_TO, upper.getValue())) throw new IllegalArgumentException();
		this.lower = lower;
		this.upper = upper;
	}
	public Range(final T lower, String rangePattern, final T upper)
	{
		Objects.requireNonNull(lower, "lower");
		Objects.requireNonNull(rangePattern, "rangePattern");
		Objects.requireNonNull(upper, "upper");
		if(ComparableSugar.is(lower, Comparison.GREATER_THAN_OR_EQUAL_TO, upper)) throw new IllegalArgumentException();
		rangePattern = rangePattern.trim();
		if(!rangePattern.matches("<?\\.\\.>?")) throw new IllegalArgumentException();

		this.lower = new Boundary<T>(lower, !rangePattern.startsWith("<"));
		this.upper = new Boundary<T>(upper, !rangePattern.endsWith(">"));
	}

	public static <T3 extends Number & Comparable<T3>> Boundary<T3> exclusive(final T3 num){return new Boundary<T3>(num, false);}
	public static <T4 extends Number & Comparable<T4>> Boundary<T4> inclusive(final T4 num){return new Boundary<T4>(num, true);}

	public boolean contains(final T testNum)
	{
		Objects.requireNonNull(testNum, "testNum");
		int compareResult = lower.getValue().compareTo(testNum);
		if(ComparableSugar.isComparisonResult(compareResult, Comparison.GREATER_THAN)) return false;
		if(ComparableSugar.isComparisonResult(compareResult, Comparison.EQUAL_TO)) return lower.isInclusive();

		compareResult = upper.getValue().compareTo(testNum);
		if(ComparableSugar.isComparisonResult(compareResult, Comparison.LESS_THAN)) return false;
		if(ComparableSugar.isComparisonResult(compareResult, Comparison.EQUAL_TO)) return upper.isInclusive();

		return true;
	}

	private static Class<?> toArrayClass(final Class<?> typeOf){return Array.newInstance(typeOf, 0).getClass();}

	public Object createArray(){return createArray(toArrayClass(lower.getValue().getClass()), lower.getIncrementAmonut());}
	public Object createArray(final T stepBy){return createArray(toArrayClass(lower.getValue().getClass()), stepBy);}
	public Object createArray(final Class<?> typeOf){return createArray(typeOf, lower.getIncrementAmonut());}
	public Object createArray(final Class<?> typeOf, final T stepBy)
	{
		Objects.requireNonNull(typeOf, "typeOf");
		Objects.requireNonNull(stepBy, "stepBy");
		if(false){if(!typeOf.isArray()) throw new IllegalArgumentException();

		final Number distance = upper.getValue().longValue() - lower.getValue().longValue();
		final Number size = distance.longValue() / stepBy.longValue();
		if(size.longValue() > Integer.MAX_VALUE) throw new IllegalArgumentException();

		final List<Number> result = new ArrayList<>();
		T index = lower.getValue();
		if(!lower.isInclusive()) index = lower.add(index, stepBy);
		for(index = lower.getValue(); index.longValue() < upper.getValue().longValue(); index = lower.add(index, stepBy))
		{
			result.add(index);
		}
		if(upper.isInclusive()) result.add(index);

		//if(typeOf == byte[].class){}
		//if(typeOf == short[].class){}
		//if(typeOf == int[].class){}
		if(typeOf == long[].class){}

		return result.toArray();}
		return null;
	}
	//TODO: method stub

	public List<?> createList(){return createList(lower.getValue().getClass(), lower.getIncrementAmonut());}
	public List<?> createList(final T stepBy){return createList(lower.getValue().getClass(), stepBy);}
	public List<?> createList(final Class<?> typeOf){return createList(typeOf, lower.getIncrementAmonut());}
	public List<?> createList(final Class<?> typeOf, final T stepBy)
	{
		final List<?> fixedSizeList = Arrays.asList(createArray(toArrayClass(typeOf), stepBy));
		return new ArrayList<>(fixedSizeList);
	}

	@Override
	public String toString()
	{
		final StringBuilder result = new StringBuilder();
		result.append(lower.getValue().toString());
		result.append(' ');

		if(!lower.isInclusive()) result.append('<');
		result.append("..");
		if(!upper.isInclusive()) result.append('>');

		result.append(' ');
		result.append(upper.getValue().toString());
		return result.toString();
	}

	//TODO: make different Boundary classes for long, double, and Number
	public final static class Boundary<T2 extends Number & Comparable<T2>>
	{
		private final T2 value;
		private final boolean inclusive;
		private Boundary(final T2 value, final boolean inclusive)
		{
			this.value = value;
			this.inclusive = inclusive;
		}

		public boolean isInclusive(){return inclusive;}
		public T2 getValue(){return value;}

		//TODO: method stub
		public T2 getIncrementAmonut(){return (T2) Integer.valueOf(1);}
		//TODO: method stub
		public T2 add(final T2 starting, final T2 stepBy){return (T2) Long.valueOf(starting.longValue() + stepBy.longValue());}
	}

}
