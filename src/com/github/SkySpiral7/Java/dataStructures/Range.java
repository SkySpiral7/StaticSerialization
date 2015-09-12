package com.github.SkySpiral7.Java.dataStructures;

import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import com.github.SkySpiral7.Java.numbers.InfiniteInteger;
import com.github.SkySpiral7.Java.pojo.Comparison;
import com.github.SkySpiral7.Java.util.ComparableSugar;

public final class Range<T extends Number & Comparable<T>>
{
	private final Boundary<T> lower;
	private final Boundary<T> upper;
	private final BoundaryInfo<T> boundaryInfo;

	public Range(final Boundary<T> lower, final Boundary<T> upper)
	{
		//TODO: clean up class and add JavaDoc to all
		Objects.requireNonNull(lower, "lower");
		Objects.requireNonNull(upper, "upper");
		if(ComparableSugar.is(lower.getValue(), Comparison.GREATER_THAN_OR_EQUAL_TO, upper.getValue())) throw new IllegalArgumentException();
		this.lower = lower;
		this.upper = upper;
		@SuppressWarnings("unchecked")
		final Class<T> temp = (Class<T>) lower.getValue().getClass();
		boundaryInfo = new BoundaryInfo<T>(temp);
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
		@SuppressWarnings("unchecked")
		final Class<T> temp = (Class<T>) lower.getClass();
		boundaryInfo = new BoundaryInfo<T>(temp);
	}

	public static <T3 extends Number & Comparable<T3>> Boundary<T3> exclusive(final T3 num){return new Boundary<T3>(num, false);}
	public static <T3 extends Number & Comparable<T3>> Boundary<T3> inclusive(final T3 num){return new Boundary<T3>(num, true);}

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

	@SuppressWarnings("unchecked")
	private static <T3> Class<T3[]> toArrayClass(final Class<T3> typeOf){return (Class<T3[]>) Array.newInstance(typeOf, 0).getClass();}

	public T[] createArray(){return createArray(toArrayClass(boundaryInfo.type), boundaryInfo.getOne());}
	public T[] createArray(final T stepBy){return createArray(toArrayClass(boundaryInfo.type), stepBy);}
	public <T3> T3 createArray(final Class<T3> typeOf){return createArray(typeOf, boundaryInfo.getOne());}
	public <T3> T3 createArray(final Class<T3> typeOf, final T stepBy)
	{
		Objects.requireNonNull(typeOf, "typeOf");
		Objects.requireNonNull(stepBy, "stepBy");
		if(!typeOf.isArray()) throw new IllegalArgumentException();

		final List<Number> numberList = new ArrayList<>();
		T index = lower.getValue();
		if(!lower.isInclusive()) index = boundaryInfo.add(index, stepBy);
		while(ComparableSugar.is(index, Comparison.LESS_THAN, upper.getValue()))
		{
			numberList.add(index);
			index = boundaryInfo.add(index, stepBy);
		}
		if(upper.isInclusive()) numberList.add(index);
		final Number[] numberArray = numberList.toArray(new Number[numberList.size()]);

		final Class<?> componentType = typeOf.getComponentType();
		@SuppressWarnings("unchecked")
		final T3 result = (T3) Array.newInstance(componentType, numberArray.length);

		for (int i = 0; i < numberArray.length; i++)
		{
			Array.set(result, i, convertTo(numberArray[i], componentType));
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	private static <T3> T3 convertTo(final Number original, final Class<T3> dest)
	{
		if(dest == byte.class || dest == Byte.class) return (T3) Byte.valueOf(original.byteValue());
		if(dest == short.class || dest == Short.class) return (T3) Short.valueOf(original.shortValue());
		if(dest == int.class || dest == Integer.class) return (T3) Integer.valueOf(original.intValue());
		if(dest == long.class || dest == Long.class) return (T3) Long.valueOf(original.longValue());
		if(dest == float.class || dest == Float.class) return (T3) Float.valueOf(original.floatValue());
		if(dest == double.class || dest == Double.class) return (T3) Double.valueOf(original.doubleValue());
		return dest.cast(original);
	}

	public List<T> createList(){return createList(boundaryInfo.type, boundaryInfo.getOne());}
	public List<T> createList(final T stepBy){return createList(boundaryInfo.type, stepBy);}
	public <T3> List<T3> createList(final Class<T3> typeOf){return createList(typeOf, boundaryInfo.getOne());}
	public <T3> List<T3> createList(final Class<T3> typeOf, final T stepBy)
	{
		final List<T3> fixedSizeList = Arrays.asList((T3[]) createArray(toArrayClass(typeOf), stepBy));
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
	}

	private final static class BoundaryInfo<T4 extends Number & Comparable<T4>>
	{
		public final Class<T4> type;
		public BoundaryInfo(final Class<T4> type){this.type = type;}

		@SuppressWarnings("unchecked")
		public T4 getOne()
		{
			//unfortunately there would be cast class exceptions for the boxes if I tried to just return (int) 1
			if(type == Byte.class) return (T4) Byte.valueOf((byte) 1);
			if(type == Short.class) return (T4) Short.valueOf((short) 1);
			if(type == Integer.class) return (T4) Integer.valueOf(1);
			if(type == Long.class) return (T4) Long.valueOf(1);
			if(type == Float.class) return (T4) Float.valueOf(1f);
			if(type == Double.class) return (T4) Double.valueOf(1d);

			if(type == BigInteger.class) return (T4) BigInteger.ONE;
			if(type == BigDecimal.class) return (T4) BigDecimal.ONE;

			if(type == InfiniteInteger.class) return (T4) InfiniteInteger.ONE;
			//MutableInfiniteInteger shouldn't be used because it can violate invariants

			throw new IllegalArgumentException(type + " is not supported");
		}

		@SuppressWarnings("unchecked")
		public T4 add(final T4 starting, final T4 stepBy)
		{
			if(type == Byte.class) return (T4) Byte.valueOf((byte) (starting.byteValue() + stepBy.byteValue()));
			if(type == Short.class) return (T4) Short.valueOf((short) (starting.shortValue() + stepBy.shortValue()));
			if(type == Integer.class) return (T4) Integer.valueOf(starting.intValue() + stepBy.intValue());
			if(type == Long.class) return (T4) Long.valueOf(starting.longValue() + stepBy.longValue());
			if(type == Float.class) return (T4) Float.valueOf(starting.floatValue() + stepBy.floatValue());
			if(type == Double.class) return (T4) Double.valueOf(starting.doubleValue() + stepBy.doubleValue());

			if(type == BigInteger.class) return (T4) ((BigInteger)starting).add((BigInteger)stepBy);
			if(type == BigDecimal.class) return (T4) ((BigDecimal)starting).add((BigDecimal)stepBy);

			if(type == InfiniteInteger.class) return (T4) ((InfiniteInteger)starting).add((InfiniteInteger)stepBy);
			//MutableInfiniteInteger shouldn't be used because it can violate invariants

			throw new IllegalArgumentException(type + " is not supported");
		}
	}

}
