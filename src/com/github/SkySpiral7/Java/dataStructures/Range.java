package com.github.SkySpiral7.Java.dataStructures;

import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import com.github.SkySpiral7.Java.numbers.AbstractInfiniteInteger;
import com.github.SkySpiral7.Java.numbers.InfiniteInteger;
import com.github.SkySpiral7.Java.numbers.MutableInfiniteInteger;
import com.github.SkySpiral7.Java.pojo.Comparison;
import com.github.SkySpiral7.Java.util.ComparableSugar;

/**
 * This immutable class represents a range between 2 numbers.
 * The range can be used to determine if a number lies within it and it can create
 * an array or list of all numbers in range. The 2 ends may be included or excluded.
 * Note that null is not allowed for any method.
 *
 * @param <T_Range> the type of number that the range represents.
 * The range supports all JRE classes that extend Number (and are self Comparable)
 * and also InfiniteInteger. Mutable numbers can't be used as they could disrupt
 * the invariants of Range.
 *
 * @see #Range(Boundary, Boundary)
 * @see #Range(Number, String, Number)
 * @see InfiniteInteger
 * @see BoundaryInfo BoundaryInfo for details on how to get range to support more number classes
 */
public final class Range<T_Range extends Number & Comparable<T_Range>>
{
	private final Boundary<T_Range> lower;
	private final Boundary<T_Range> upper;
	/**This is the same for both lower and upper since they must be the same class.*/
	private final BoundaryInfo<T_Range> boundaryInfo;

	/**
	 * Create a new Range with the bounds given.
	 * See inclusive/exclusive in order to create the Boundary objects.
	 *
	 * @param lower the lower bound for the number range
	 * @param upper the upper bound for the number range
	 *
	 * @throws IllegalArgumentException if lower &gt;= upper,
	 * if lower or upper are NaN,
	 * or if T_Range is mutable.
	 *
	 * @see #inclusive(Number)
	 * @see #exclusive(Number)
	 * @see #Range(Number, String, Number)
	 */
	public Range(final Boundary<T_Range> lower, final Boundary<T_Range> upper)
	{
		Objects.requireNonNull(lower, "lower");
		Objects.requireNonNull(upper, "upper");
		this.lower = lower;
		this.upper = upper;
		@SuppressWarnings("unchecked")
		final Class<T_Range> temp = (Class<T_Range>) lower.getValue().getClass();
		boundaryInfo = new BoundaryInfo<T_Range>(temp);

		checkInvariants();
	}
	/**
	 * <p>Create a new Range with the bounds given.
	 * rangePattern is used to determine which bounds are inclusive/exclusive, see list below
	 * (without the quotes, leading and trailing whitespace is ignored).</p>
	 *
	 * <ul>
	 * <li>{@code ".."} to have both bounds included</li>
	 * <li>{@code "<.."} to have lower excluded and upper included</li>
	 * <li>{@code "..>"} to have lower included and upper excluded</li>
	 * <li>{@code "<..>"} to have both bounds excluded</li>
	 * </ul>
	 *
	 * <p>Examples: {@code new Range<Integer>(0, "..>", 10)} to represent all legal indexes for an array with length 10.
	 * {@code new Range<Float>(0.0f, "<..>", Float.POSITIVE_INFINITY)} to represent all non-negative finite floating point numbers.</p>
	 *
	 * @param lower the lower bound for the number range
	 * @param rangePattern one of the four strings as listed above
	 * @param upper the upper bound for the number range
	 *
	 * @throws IllegalArgumentException if lower &gt;= upper,
	 * if rangePattern has an invalid value,
	 * if lower or upper are NaN,
	 * or if T_Range is mutable.
	 *
	 * @see #Range(Boundary, Boundary)
	 */
	public Range(final T_Range lower, String rangePattern, final T_Range upper)
	{
		Objects.requireNonNull(lower, "lower");
		Objects.requireNonNull(rangePattern, "rangePattern");
		Objects.requireNonNull(upper, "upper");
		if(!rangePattern.matches("\\s*<?\\.\\.>?\\s*")) throw new IllegalArgumentException("Bad format for rangePattern: " + rangePattern);
		rangePattern = rangePattern.trim();

		this.lower = new Boundary<T_Range>(lower, !rangePattern.startsWith("<"));
		this.upper = new Boundary<T_Range>(upper, !rangePattern.endsWith(">"));
		@SuppressWarnings("unchecked")
		final Class<T_Range> temp = (Class<T_Range>) lower.getClass();
		boundaryInfo = new BoundaryInfo<T_Range>(temp);

		checkInvariants();
	}

	/**
	 * @throws IllegalArgumentException if lower &gt;= upper,
	 * if lower or upper are NaN,
	 * or if T_Range is mutable.
	 */
	private void checkInvariants()
	{
		checkForNanOfKnownClasses(lower.getValue());
		checkForNanOfKnownClasses(upper.getValue());
		checkKnownMutableClasses(boundaryInfo.type);

		//checked last so that illegal class will throw first (or NaN)
		if(ComparableSugar.is(lower.getValue(), Comparison.GREATER_THAN_OR_EQUAL_TO, upper.getValue()))
			throw new IllegalArgumentException("lower >= upper (" + lower.getValue() + " >= " + upper.getValue() + ")");
	}

	/**
	 * @throws IllegalArgumentException if num is NaN
	 */
	private void checkForNanOfKnownClasses(final T_Range num)
	{
		boolean isLegal = true;

		if (boundaryInfo.type.equals(Float.class))
		{
			if(Float.isNaN((Float) num)) isLegal = false;
		}
		else if (boundaryInfo.type.equals(Double.class))
		{
			if(Double.isNaN((Double) num)) isLegal = false;
		}
		else if (boundaryInfo.type.equals(InfiniteInteger.class))
		{
			if(((InfiniteInteger) num).isNaN()) isLegal = false;
		}

		if(!isLegal) throw new IllegalArgumentException("NaN can't be a boundary");
	}
	/**
	 * @throws IllegalArgumentException if class1 is mutable
	 */
	private static void checkKnownMutableClasses(final Class<?> class1)
	{
		boolean isLegal = true;
		isLegal &= (class1 != MutableInfiniteInteger.class);  //mutable
		isLegal &= (class1 != AbstractInfiniteInteger.class);  //could be mutable

		if(!isLegal) throw new IllegalArgumentException(class1 + " is mutable");
	}

	/**
	 * @return a Boundary that includes the number passed in.
	 *
	 * @see #Range(Boundary, Boundary)
	 * @see #exclusive(Number)
	 */
	public static <T_New extends Number & Comparable<T_New>> Boundary<T_New> inclusive(final T_New num)
	{
		return new Boundary<T_New>(num, true);
	}
	/**
	 * @return a Boundary that excludes the number passed in.
	 *
	 * @see #Range(Boundary, Boundary)
	 * @see #inclusive(Number)
	 */
	public static <T_New extends Number & Comparable<T_New>> Boundary<T_New> exclusive(final T_New num)
	{
		return new Boundary<T_New>(num, false);
	}

	/**
	 * @return true if testNum exists within the number range that this class represents
	 */
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

	/**
	 * Converts {@code Class<T_Custom> typeOf} to {@code Class<T_Custom[]>}.
	 */
	@SuppressWarnings("unchecked")
	private static <T_Custom> Class<T_Custom[]> toArrayClass(final Class<T_Custom> typeOf)
	{
		return (Class<T_Custom[]>) Array.newInstance(typeOf, 0).getClass();
	}

	/**
	 * Simply calls createArray(T_Range[].class, 1).
	 * @return an array of all numbers (counting by 1) included in this range
	 * @see #createArray(Class, Number)
	 */
	public T_Range[] createArray(){return createArray(toArrayClass(boundaryInfo.type), boundaryInfo.getOne());}
	/**
	 * Simply calls createArray(T_Range[].class, stepBy).
	 * @return an array of all numbers (counting by stepBy) included in this range
	 * @see #createArray(Class, Number)
	 */
	public T_Range[] createArray(final T_Range stepBy){return createArray(toArrayClass(boundaryInfo.type), stepBy);}
	/**
	 * Simply calls createArray(typeOf, 1).
	 * @return an array of all numbers (counting by 1) included in this range
	 * @see #createArray(Class, Number)
	 */
	public <T_Custom> T_Custom createArray(final Class<T_Custom> typeOf){return createArray(typeOf, boundaryInfo.getOne());}
	/**
	 * <p>Creates an array of type {@code typeOf} of all numbers included in this range counting by {@code stepBy}.
	 * The numbers start with the lower bound (if it is included) and keep adding stepBy until the number is greater than
	 * the upper bound (that last number is never included).</p>
	 *
	 * <p>{@code typeOf} must be a class of an array class (primitive or object). All numbers included in the array (if
	 * not empty) will be cast from T_Range to the component type of {@code typeOf} therefore a class cast exception
	 * may occur or the numbers may be truncated.</p>
	 *
	 * <p>If this Range's lower bound is -&infin; and/or upper bound is +&infin; then this method
	 * will eventually run out of memory. This will happen any time you try to store more than 2^31-1 elements
	 * since arrays can't be that large. Likewise there is no check to see if stepBy &lt;= 0 (or NaN) which would
	 * also be endless.</p>
	 *
	 * <p>Examples: {@code new Range<Integer>(0, "..", 255).createArray(byte[].class, 1)} returns a primitive byte array
	 * of all byte values (casting from int to byte fits but causes them to be signed).
	 * {@code new Range<Double>(0.0d, "..", 1.0d).createArray(Float[].class, 0.5d)} returns {@code new Float[]{0.0f, 0.5f, 1.0f}}.
	 * {@code new Range<Integer>(1, "<..", 10).createArray(short[].class, 1000)} returns an empty array.</p>
	 *
	 * @param typeOf the class of the array to be returned
	 * @param stepBy the amount to add between each number
	 *
	 * @return an array of all numbers (counting by stepBy) included in this range
	 *
	 * @throws IllegalArgumentException if typeOf isn't an array class or if the component type of typeOf is not supported
	 *
	 * @see #createList(Class, Number)
	 */
	public <T_Custom> T_Custom createArray(final Class<T_Custom> typeOf, final T_Range stepBy)
	{
		Objects.requireNonNull(typeOf, "typeOf");
		Objects.requireNonNull(stepBy, "stepBy");
		if(!typeOf.isArray()) throw new IllegalArgumentException(typeOf + " isn't an array class");

		final Number[] numberArray = createStream(stepBy).toArray(Number[]::new);
		//I can't have the stream convert to primitive so I do that myself below

		final Class<?> componentType = typeOf.getComponentType();
		@SuppressWarnings("unchecked")
		final T_Custom result = (T_Custom) Array.newInstance(componentType, numberArray.length);

		for (int i = 0; i < numberArray.length; i++)
		{
			Array.set(result, i, convertTo(numberArray[i], componentType));
		}
		return result;
	}

	/**
	 * Converts original into an object of class dest. Doing so may truncate or throw.
	 */
	@SuppressWarnings("unchecked")
	private static <T_Custom> T_Custom convertTo(final Number original, final Class<T_Custom> dest)
	{
		//need to check for both primitive and box classes so that either array type can be used
		//although it is only possible to return boxes they'll be auto-unboxed later if needed
		if(dest == byte.class || dest == Byte.class) return (T_Custom) Byte.valueOf(original.byteValue());
		if(dest == short.class || dest == Short.class) return (T_Custom) Short.valueOf(original.shortValue());
		if(dest == int.class || dest == Integer.class) return (T_Custom) Integer.valueOf(original.intValue());
		if(dest == long.class || dest == Long.class) return (T_Custom) Long.valueOf(original.longValue());
		if(dest == float.class || dest == Float.class) return (T_Custom) Float.valueOf(original.floatValue());
		if(dest == double.class || dest == Double.class) return (T_Custom) Double.valueOf(original.doubleValue());
		return dest.cast(original);
	}

	/**
	 * Simply calls createList(T_Range.class, 1).
	 * @return a list of all numbers (counting by 1) included in this range
	 * @see #createList(Class, Number)
	 */
	public List<T_Range> createList(){return createList(boundaryInfo.type, boundaryInfo.getOne());}
	/**
	 * Simply calls createList(T_Range.class, stepBy).
	 * @return a list of all numbers (counting by stepBy) included in this range
	 * @see #createList(Class, Number)
	 */
	public List<T_Range> createList(final T_Range stepBy){return createList(boundaryInfo.type, stepBy);}
	/**
	 * Simply calls createList(typeOf, 1).
	 * @return a list of all numbers (counting by 1) included in this range
	 * @see #createList(Class, Number)
	 */
	public <T_Custom> List<T_Custom> createList(final Class<T_Custom> typeOf){return createList(typeOf, boundaryInfo.getOne());}
	/**
	 * <p>Creates a list of type {@code typeOf} of all numbers included in this range counting by {@code stepBy}.
	 * The numbers start with the lower bound (if it is included) and keep adding stepBy until the number is greater than
	 * the upper bound (that last number is never included).</p>
	 *
	 * <p>All numbers included in the list (if
	 * not empty) will be cast from T_Range to {@code typeOf} therefore a class cast exception
	 * may occur or the numbers may be truncated.</p>
	 *
	 * <p>If this Range's lower bound is -&infin; and/or upper bound is +&infin; then this method
	 * will eventually run out of memory. This will happen any time you try to store more than 2^31-1 elements
	 * since arrays can't be that large (the list is backed by an array). Likewise there is no check to see if
	 * stepBy &lt;= 0 (or NaN) which would also be endless.</p>
	 *
	 * <p>For example: {@code new Range<Integer>(256, "..", 258).createList(Byte.class, 1)} returns a list of 3 elements
	 * however each of them will have been truncated when cast from int to byte.</p>
	 *
	 * @param typeOf the class of the elements of the returned list (primitives will be auto-boxed)
	 * @param stepBy the amount to add between each number
	 *
	 * @return a list of all numbers (counting by stepBy) included in this range
	 *
	 * @throws IllegalArgumentException if typeOf is not a supported class
	 *
	 * @see #createArray(Class, Number)
	 */
	public <T_Custom> List<T_Custom> createList(final Class<T_Custom> typeOf, final T_Range stepBy)
	{
		return createStream(typeOf, stepBy).collect(Collectors.toCollection(ArrayList::new));
	}

	/**
	 * Same as createStream(T_Range.class, 1).
	 * @return a stream of all numbers (counting by 1) included in this range
	 * @see #createStream(Class, Number)
	 */
	public Stream<T_Range> createStream(){return createStream(boundaryInfo.getOne());}
	/**
	 * Same as createStream(T_Range.class, stepBy).
	 * @return a stream of all numbers (counting by stepBy) included in this range
	 * @see #createStream(Class, Number)
	 */
	public Stream<T_Range> createStream(final T_Range stepBy)
	{
		Objects.requireNonNull(stepBy, "stepBy");
		T_Range index = lower.getValue();
		if(!lower.isInclusive()) index = boundaryInfo.add(index, stepBy);

     return StreamSupport.stream(Spliterators.spliteratorUnknownSize(
             new IterateByStep(index, stepBy),
             Spliterator.ORDERED | Spliterator.IMMUTABLE), false);
	}
	/**
	 * Simply calls createStream(typeOf, 1).
	 * @return a stream of all numbers (counting by 1) included in this range
	 * @see #createStream(Class, Number)
	 */
	public <T_Custom> Stream<T_Custom> createStream(final Class<T_Custom> typeOf){return createStream(typeOf, boundaryInfo.getOne());}
	/**
	 * <p>Creates a stream of type {@code typeOf} of all numbers included in this range counting by {@code stepBy}.
	 * The numbers start with the lower bound (if it is included) and keep adding stepBy until the number is greater than
	 * the upper bound (that last number is never included).</p>
	 *
	 * <p>All numbers included in the stream (if
	 * not empty) will be cast from T_Range to {@code typeOf} therefore a class cast exception
	 * may occur or the numbers may be truncated.</p>
	 *
	 * <p>If this Range's lower bound is -&infin; and/or upper bound is +&infin; then the stream
	 * will not end. Likewise there is no check to see if
	 * stepBy &lt;= 0 (or NaN) which would also be endless.</p>
	 *
	 * @param typeOf the class of the elements in the returned list (primitives will be auto-boxed)
	 * @param stepBy the amount to add between each number
	 *
	 * @return a stream of all numbers (counting by stepBy) included in this range
	 *
	 * @throws IllegalArgumentException if typeOf is not a supported class
	 *
	 * @see #createList(Class, Number)
	 */
	public <T_Custom> Stream<T_Custom> createStream(final Class<T_Custom> typeOf, final T_Range stepBy)
	{
		Objects.requireNonNull(typeOf, "typeOf");
		Objects.requireNonNull(stepBy, "stepBy");  //redundant but easier to understand stack trace
     return createStream(stepBy).map((T_Range input) -> {return convertTo(input, typeOf);});
	}

	private final class IterateByStep implements Iterator<T_Range>
	{
      private T_Range index;
      private final T_Range stepBy;
      public IterateByStep(final T_Range index, final T_Range stepBy)
      {
      	this.index = index;
      	this.stepBy = stepBy;
      }

      @Override public boolean hasNext(){return Range.this.contains(index);}

      @Override
      public T_Range next() {
      	if(!hasNext()) throw new NoSuchElementException();
      	final T_Range returnValue = index;

      	index = boundaryInfo.add(index, stepBy);

   		return returnValue;
      }
  };

  /**
   * Returns a string representation of the object.
   *
   * @return "lower rangePattern upper" where lower and upper are the lower bound and upper bound
   * converted to strings and rangePattern matches the format described by Range(Number, String, Number)
   *
   * @see #Range(Number, String, Number)
   */
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

	/**Standard getter for the lower bound.*/
	public Boundary<T_Range> getLowerBound(){return lower;}
	/**Standard getter for the upper bound.*/
	public Boundary<T_Range> getUpperBound(){return upper;}

	/**
	 * An instance of this class represents one of the boundaries of Range.
	 * This is simply a bean that holds the number value and an inclusive flag.
	 *
	 * @param <T_Boundary> the type of number stored
	 *
	 * @see Range
	 * @see Range#inclusive(Number)
	 * @see Range#exclusive(Number)
	 */
	public final static class Boundary<T_Boundary extends Number & Comparable<T_Boundary>>
	{
		private final T_Boundary value;
		private final boolean inclusive;
		/**private so that they are forced to use the 2 factors which is more readable than a boolean.*/
		private Boundary(final T_Boundary value, final boolean inclusive)
		{
			this.value = value;
			this.inclusive = inclusive;
		}

		/**Standard getter for the value of the boundary.*/
		public T_Boundary getValue(){return value;}
		/**
		 * @return true if the Range of numbers includes the value of this boundary
		 */
		public boolean isInclusive(){return inclusive;}
	}

	/**
	 * This class describes and operates on the numbers used by Range. In order to add another class
	 * so that Range can use it you only need to add a single line to each method.
	 *
	 * @param <T_BoundaryInfo> the type of number stored in Range
	 *
	 * @see #add(Number, Number)
	 * @see #getOne()
	 */
	private final static class BoundaryInfo<T_BoundaryInfo extends Number & Comparable<T_BoundaryInfo>>
	{
		/**The class that Range was created with. Stored explicitly for array and list creation.*/
		public final Class<T_BoundaryInfo> type;
		public BoundaryInfo(final Class<T_BoundaryInfo> type){this.type = type;}

		/**
		 * This method is used by each of the linked methods in order to get the default stepBy amount.
		 * This amount should be 1 if possible in order to be consistent.
		 * If this method doesn't support a given class then the other createArray and list methods can still be used
		 * since they explicitly pass in stepBy.
		 *
		 * @return the value 1 in order to add by that amount
		 *
		 * @throws IllegalArgumentException if T_BoundaryInfo doesn't allow a default stepBy amount
		 *
		 * @see Range#createArray()
		 * @see Range#createArray(Class)
		 * @see Range#createList()
		 * @see Range#createList(Class)
		 */
		@SuppressWarnings("unchecked")
		public T_BoundaryInfo getOne()
		{
			//unfortunately there would be cast class exceptions for the boxes if I tried to just return (int) 1
			//don't need to check for the primitive classes since Range couldn't've been created with them
			if(type.equals(Byte.class)) return (T_BoundaryInfo) Byte.valueOf((byte) 1);
			if(type.equals(Short.class)) return (T_BoundaryInfo) Short.valueOf((short) 1);
			if(type.equals(Integer.class)) return (T_BoundaryInfo) Integer.valueOf(1);
			if(type.equals(Long.class)) return (T_BoundaryInfo) Long.valueOf(1L);
			if(type.equals(Float.class)) return (T_BoundaryInfo) Float.valueOf(1f);
			if(type.equals(Double.class)) return (T_BoundaryInfo) Double.valueOf(1d);

			if(type.equals(BigInteger.class)) return (T_BoundaryInfo) BigInteger.ONE;
			if(type.equals(BigDecimal.class)) return (T_BoundaryInfo) BigDecimal.ONE;

			if(type.equals(InfiniteInteger.class)) return (T_BoundaryInfo) InfiniteInteger.ONE;
			//MutableInfiniteInteger shouldn't be used because it can violate invariants
			//likewise for AbstractInfiniteInteger which could be mutable

			throw new IllegalArgumentException("stepBy is required for " + type);
		}

		/**
		 * This method is used by every createArray and list method so that changing between each number is possible.
		 *
		 * @param starting the previous value already used (or not)
		 * @param stepBy the amount to add to get to the next number to be used (or not)
		 *
		 * @return the next number in the sequence
		 *
		 * @throws IllegalArgumentException if T_BoundaryInfo can't be made into an array or list
		 */
		@SuppressWarnings("unchecked")
		public T_BoundaryInfo add(final T_BoundaryInfo starting, final T_BoundaryInfo stepBy)
		{
			//don't need to check for the primitive classes since Range couldn't've been created with them
			if(type.equals(Byte.class)) return (T_BoundaryInfo) Byte.valueOf((byte) (starting.byteValue() + stepBy.byteValue()));
			if(type.equals(Short.class)) return (T_BoundaryInfo) Short.valueOf((short) (starting.shortValue() + stepBy.shortValue()));
			if(type.equals(Integer.class)) return (T_BoundaryInfo) Integer.valueOf(starting.intValue() + stepBy.intValue());
			if(type.equals(Long.class)) return (T_BoundaryInfo) Long.valueOf(starting.longValue() + stepBy.longValue());
			if(type.equals(Float.class)) return (T_BoundaryInfo) Float.valueOf(starting.floatValue() + stepBy.floatValue());
			if(type.equals(Double.class)) return (T_BoundaryInfo) Double.valueOf(starting.doubleValue() + stepBy.doubleValue());

			if(type.equals(BigInteger.class)) return (T_BoundaryInfo) ((BigInteger)starting).add((BigInteger)stepBy);
			if(type.equals(BigDecimal.class)) return (T_BoundaryInfo) ((BigDecimal)starting).add((BigDecimal)stepBy);

			if(type.equals(InfiniteInteger.class)) return (T_BoundaryInfo) ((InfiniteInteger)starting).add((InfiniteInteger)stepBy);
			//MutableInfiniteInteger shouldn't be used because it can violate invariants
			//likewise for AbstractInfiniteInteger which could be mutable

			throw new IllegalArgumentException("Can't create an array for " + type);
		}
	}

}
