package com.github.SkySpiral7.Java.dataStructures;

import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import com.github.SkySpiral7.Java.numbers.InfiniteInteger;
import com.github.SkySpiral7.Java.pojo.Comparison;
import com.github.SkySpiral7.Java.util.ComparableSugar;

public final class Range<T_Range extends Number & Comparable<T_Range>>
{
	private final Boundary<T_Range> lower;
	private final Boundary<T_Range> upper;
	private final BoundaryInfo<T_Range> boundaryInfo;

	public Range(final Boundary<T_Range> lower, final Boundary<T_Range> upper)
	{
		//TODO: add JavaDoc to all
		Objects.requireNonNull(lower, "lower");
		Objects.requireNonNull(upper, "upper");
		if(ComparableSugar.is(lower.getValue(), Comparison.GREATER_THAN_OR_EQUAL_TO, upper.getValue()))
			throw new IllegalArgumentException("lower >= upper (" + lower.getValue() + " >= " + upper.getValue() + ")");
		this.lower = lower;
		this.upper = upper;
		@SuppressWarnings("unchecked")
		final Class<T_Range> temp = (Class<T_Range>) lower.getValue().getClass();
		boundaryInfo = new BoundaryInfo<T_Range>(temp);
	}
	public Range(final T_Range lower, String rangePattern, final T_Range upper)
	{
		Objects.requireNonNull(lower, "lower");
		Objects.requireNonNull(rangePattern, "rangePattern");
		Objects.requireNonNull(upper, "upper");
		if(ComparableSugar.is(lower, Comparison.GREATER_THAN_OR_EQUAL_TO, upper))
			throw new IllegalArgumentException("lower >= upper (" + lower + " >= " + upper + ")");
		if(!rangePattern.matches("\\s*<?\\.\\.>?\\s*")) throw new IllegalArgumentException("Bad format for rangePattern: " + rangePattern);
		rangePattern = rangePattern.trim();

		this.lower = new Boundary<T_Range>(lower, !rangePattern.startsWith("<"));
		this.upper = new Boundary<T_Range>(upper, !rangePattern.endsWith(">"));
		@SuppressWarnings("unchecked")
		final Class<T_Range> temp = (Class<T_Range>) lower.getClass();
		boundaryInfo = new BoundaryInfo<T_Range>(temp);
	}

	public static <T_New extends Number & Comparable<T_New>> Boundary<T_New> inclusive(final T_New num)
	{
		return new Boundary<T_New>(num, true);
	}
	public static <T_New extends Number & Comparable<T_New>> Boundary<T_New> exclusive(final T_New num)
	{
		return new Boundary<T_New>(num, false);
	}

	public boolean contains(final T_Range testNum)
	{
		Objects.requireNonNull(testNum, "testNum");
		int compareResult = testNum.compareTo(lower.getValue());
		if(ComparableSugar.isComparisonResult(compareResult, Comparison.LESS_THAN)) return false;
		if(ComparableSugar.isComparisonResult(compareResult, Comparison.EQUAL_TO)) return lower.isInclusive();

		compareResult = testNum.compareTo(upper.getValue());
		if(ComparableSugar.isComparisonResult(compareResult, Comparison.GREATER_THAN)) return false;
		if(ComparableSugar.isComparisonResult(compareResult, Comparison.EQUAL_TO)) return upper.isInclusive();

		return true;
	}

	@SuppressWarnings("unchecked")
	private static <T_Custom> Class<T_Custom[]> toArrayClass(final Class<T_Custom> typeOf)
	{
		return (Class<T_Custom[]>) Array.newInstance(typeOf, 0).getClass();
	}

	public T_Range[] createArray(){return createArray(toArrayClass(boundaryInfo.type), boundaryInfo.getOne());}
	public T_Range[] createArray(final T_Range stepBy){return createArray(toArrayClass(boundaryInfo.type), stepBy);}
	public <T_Custom> T_Custom createArray(final Class<T_Custom> typeOf){return createArray(typeOf, boundaryInfo.getOne());}
	public <T_Custom> T_Custom createArray(final Class<T_Custom> typeOf, final T_Range stepBy)
	{
		Objects.requireNonNull(typeOf, "typeOf");
		Objects.requireNonNull(stepBy, "stepBy");
		if(!typeOf.isArray()) throw new IllegalArgumentException(typeOf + " isn't an array class");

		final List<Number> numberList = new ArrayList<>();
		T_Range index = lower.getValue();
		if(!lower.isInclusive()) index = boundaryInfo.add(index, stepBy);
		while(ComparableSugar.is(index, Comparison.LESS_THAN, upper.getValue()))
		{
			numberList.add(index);
			index = boundaryInfo.add(index, stepBy);
		}
		//if stepBy > 1 then index could be > upper. other possible ways for boundaryInfo.type of float etc
		if(upper.isInclusive() && index.equals(upper.getValue())) numberList.add(index);
		//although I could use numberList, calling toArray is fast, numberArray is smaller, and numberArray has faster iterations below
		final Number[] numberArray = numberList.toArray(new Number[numberList.size()]);

		final Class<?> componentType = typeOf.getComponentType();
		@SuppressWarnings("unchecked")
		final T_Custom result = (T_Custom) Array.newInstance(componentType, numberArray.length);

		for (int i = 0; i < numberArray.length; i++)
		{
			Array.set(result, i, convertTo(numberArray[i], componentType));
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	private static <T_Custom> T_Custom convertTo(final Number original, final Class<T_Custom> dest)
	{
		if(dest == byte.class || dest == Byte.class) return (T_Custom) Byte.valueOf(original.byteValue());
		if(dest == short.class || dest == Short.class) return (T_Custom) Short.valueOf(original.shortValue());
		if(dest == int.class || dest == Integer.class) return (T_Custom) Integer.valueOf(original.intValue());
		if(dest == long.class || dest == Long.class) return (T_Custom) Long.valueOf(original.longValue());
		if(dest == float.class || dest == Float.class) return (T_Custom) Float.valueOf(original.floatValue());
		if(dest == double.class || dest == Double.class) return (T_Custom) Double.valueOf(original.doubleValue());
		return dest.cast(original);
	}

	public List<T_Range> createList(){return createList(boundaryInfo.type, boundaryInfo.getOne());}
	public List<T_Range> createList(final T_Range stepBy){return createList(boundaryInfo.type, stepBy);}
	public <T_Custom> List<T_Custom> createList(final Class<T_Custom> typeOf){return createList(typeOf, boundaryInfo.getOne());}
	public <T_Custom> List<T_Custom> createList(final Class<T_Custom> typeOf, final T_Range stepBy)
	{
		if(typeOf.isPrimitive()) throw new IllegalArgumentException("Can't create a list of primitive " + typeOf);
		final List<T_Custom> fixedSizeList = Arrays.asList(createArray(toArrayClass(typeOf), stepBy));
		return new ArrayList<>(fixedSizeList);
	}

	//TODO: more and test
	//TODO: have createArray use this
	private Stream<T_Range> createStream(final T_Range stepBy)
	{
		T_Range index = lower.getValue();
		if(!lower.isInclusive()) index = boundaryInfo.add(index, stepBy);

     return StreamSupport.stream(Spliterators.spliteratorUnknownSize(
             new IterateByStep(index, stepBy),
             Spliterator.ORDERED | Spliterator.IMMUTABLE), false);
	}
	private final class IterateByStep implements Iterator<T_Range>
	{
      private T_Range index;
      private final T_Range stepBy;
      public IterateByStep(T_Range index, T_Range stepBy)
      {
      	this.index = index;
      	this.stepBy = stepBy;

      	//check to see if index is allowed (if not then this iterator is empty)
      	checkIndex();
      }

      @Override public boolean hasNext(){return (null != index);}

      @Override
      public T_Range next() {
      	if(!hasNext()) throw new NoSuchElementException();
      	final T_Range temp = index;
      	index = boundaryInfo.add(index, stepBy);

      	checkIndex();

   		return temp;
      }

		private void checkIndex()
		{
			if(ComparableSugar.is(index, Comparison.LESS_THAN, upper.getValue())){}  //allow index value
      	else if(upper.isInclusive() && index.equals(upper.getValue())){}
      	else index = null;  //no more values
		}
  };

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

	public Boundary<T_Range> getLowerBound(){return lower;}
	public Boundary<T_Range> getUpperBound(){return upper;}

	public final static class Boundary<T_Boundary extends Number & Comparable<T_Boundary>>
	{
		private final T_Boundary value;
		private final boolean inclusive;
		private Boundary(final T_Boundary value, final boolean inclusive)
		{
			this.value = value;
			this.inclusive = inclusive;
		}

		public boolean isInclusive(){return inclusive;}
		public T_Boundary getValue(){return value;}
	}

	private final static class BoundaryInfo<T_BoundaryInfo extends Number & Comparable<T_BoundaryInfo>>
	{
		public final Class<T_BoundaryInfo> type;
		public BoundaryInfo(final Class<T_BoundaryInfo> type){this.type = type;}

		@SuppressWarnings("unchecked")
		public T_BoundaryInfo getOne()
		{
			//unfortunately there would be cast class exceptions for the boxes if I tried to just return (int) 1
			if(type == Byte.class) return (T_BoundaryInfo) Byte.valueOf((byte) 1);
			if(type == Short.class) return (T_BoundaryInfo) Short.valueOf((short) 1);
			if(type == Integer.class) return (T_BoundaryInfo) Integer.valueOf(1);
			if(type == Long.class) return (T_BoundaryInfo) Long.valueOf(1L);
			if(type == Float.class) return (T_BoundaryInfo) Float.valueOf(1f);
			if(type == Double.class) return (T_BoundaryInfo) Double.valueOf(1d);

			if(type == BigInteger.class) return (T_BoundaryInfo) BigInteger.ONE;
			if(type == BigDecimal.class) return (T_BoundaryInfo) BigDecimal.ONE;

			if(type == InfiniteInteger.class) return (T_BoundaryInfo) InfiniteInteger.ONE;
			//MutableInfiniteInteger shouldn't be used because it can violate invariants

			throw new IllegalArgumentException("stepBy is required for " + type);
		}

		@SuppressWarnings("unchecked")
		public T_BoundaryInfo add(final T_BoundaryInfo starting, final T_BoundaryInfo stepBy)
		{
			if(type == Byte.class) return (T_BoundaryInfo) Byte.valueOf((byte) (starting.byteValue() + stepBy.byteValue()));
			if(type == Short.class) return (T_BoundaryInfo) Short.valueOf((short) (starting.shortValue() + stepBy.shortValue()));
			if(type == Integer.class) return (T_BoundaryInfo) Integer.valueOf(starting.intValue() + stepBy.intValue());
			if(type == Long.class) return (T_BoundaryInfo) Long.valueOf(starting.longValue() + stepBy.longValue());
			if(type == Float.class) return (T_BoundaryInfo) Float.valueOf(starting.floatValue() + stepBy.floatValue());
			if(type == Double.class) return (T_BoundaryInfo) Double.valueOf(starting.doubleValue() + stepBy.doubleValue());

			if(type == BigInteger.class) return (T_BoundaryInfo) ((BigInteger)starting).add((BigInteger)stepBy);
			if(type == BigDecimal.class) return (T_BoundaryInfo) ((BigDecimal)starting).add((BigDecimal)stepBy);

			if(type == InfiniteInteger.class) return (T_BoundaryInfo) ((InfiniteInteger)starting).add((InfiniteInteger)stepBy);
			//MutableInfiniteInteger shouldn't be used because it can violate invariants

			throw new IllegalArgumentException("Can't create an array for " + type);
		}
	}

}
