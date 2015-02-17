package com.github.SkySpiral7.Java;

import static com.github.SkySpiral7.Java.pojo.Comparison.GREATER_THAN;
import static com.github.SkySpiral7.Java.pojo.Comparison.GREATER_THAN_OR_EQUAL_TO;
import static com.github.SkySpiral7.Java.pojo.Comparison.LESS_THAN;
import static com.github.SkySpiral7.Java.pojo.Comparison.LESS_THAN_OR_EQUAL_TO;
import static com.github.SkySpiral7.Java.util.ComparableSugar.THIS_EQUAL;
import static com.github.SkySpiral7.Java.util.ComparableSugar.THIS_GREATER;
import static com.github.SkySpiral7.Java.util.ComparableSugar.THIS_LESSER;
import static com.github.SkySpiral7.Java.util.ComparableSugar.is;
import static com.github.SkySpiral7.Java.util.ComparableSugar.isComparisonResult;

import java.io.File;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.Objects;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import com.github.SkySpiral7.Java.iterators.DequeNodeIterator;
import com.github.SkySpiral7.Java.iterators.DescendingListIterator;
import com.github.SkySpiral7.Java.iterators.ReadOnlyListIterator;
import com.github.SkySpiral7.Java.pojo.DequeNode;
import com.github.SkySpiral7.Java.pojo.IntegerQuotient;
import com.github.SkySpiral7.Java.util.BitWiseUtil;

//aka InfinInt
//maxes: int[] 2^(32 * (2^31-1))-1 long[] 2^(64 * (2^31-1))-1
//string (base 10): 10^(2^31-1) which is much smaller; big int: ?
public class InfiniteInteger extends Number implements Copyable<InfiniteInteger>, Comparable<InfiniteInteger> {
	private static final long serialVersionUID = 1L;

	/**This constant represents 0 and it is the only InfiniteInteger that can be 0 (singleton).
	 * Therefore it is safe to use pointer equality for comparison: <code>if(infiniteIntegerVar == InfiniteInteger.ZERO)</code>*/
	public static final InfiniteInteger ZERO = new InfiniteInteger(0);
	/**Common abbreviation for "not a number". This constant is the result of invalid math such as 0/0.
	 * Note that this is a normal object such that <code>(InfiniteInteger.NaN == InfiniteInteger.NaN)</code> is
	 * always true. Therefore it is logically correct unlike the floating point unit's NaN.*/
	public static final InfiniteInteger NaN = new InfiniteInteger(false);
	/**+&infin; is a concept rather than a number but will be returned by math such as 1/0.*/
	public static final InfiniteInteger POSITIVE_INFINITITY = new InfiniteInteger(true);
	/**-&infin; is a concept rather than a number but will be returned by math such as -1/0.*/
	public static final InfiniteInteger NEGATIVE_INFINITITY = new InfiniteInteger(false);

	/**
	 * Little endian: the first node is the least significant.
	 */
	protected DequeNode<Integer> magnitudeHead;
	protected boolean isNegative;

	/**
	 * This constructor is used to make special constants.
	 * My making the head null it does something the other constructor can't.
	 * @param isNegative used for positive and negative infinity. Meaningless to NaN.
	 */
	protected InfiniteInteger(boolean isNegative){magnitudeHead = null; this.isNegative = isNegative;}
	protected InfiniteInteger(long value) {
		if (value == Long.MIN_VALUE)
		{
			isNegative = true;
			magnitudeHead = DequeNode.Factory.createStandAloneNode(0);
			DequeNode.Factory.createNodeAfter(magnitudeHead, Integer.MIN_VALUE);
		}
		else
		{
			isNegative = (value < 0);
			value = Math.abs(value);
			magnitudeHead = DequeNode.Factory.createStandAloneNode((int) value);
			value >>>= 32;
			if(value > 0) DequeNode.Factory.createNodeAfter(magnitudeHead, ((int) value));
		}
	}

	public static InfiniteInteger valueOf(long value) {
		if(value == 0) return ZERO;
		return new InfiniteInteger(value);
	}

	public static InfiniteInteger valueOf(BigInteger value) {
		boolean willBeNegative = (value.signum() == -1);  //don't need to use < 0 because of signum's promise
		BigInteger valueRemaining = value.abs();

		final BigInteger bigIntegerMaxLong = BigInteger.valueOf(Long.MAX_VALUE);
		if(is(valueRemaining, LESS_THAN_OR_EQUAL_TO, bigIntegerMaxLong)) return InfiniteInteger.valueOf(value.longValue());
		//if abs fits in a signed long then delegate

		InfiniteInteger result = InfiniteInteger.ZERO;
		if(value.equals(BigInteger.ZERO)) return InfiniteInteger.valueOf(value.longValue());

		while (is(valueRemaining, GREATER_THAN, bigIntegerMaxLong))
		{
			result = result.add(Long.MAX_VALUE);
			valueRemaining = valueRemaining.subtract(bigIntegerMaxLong);
		}
		result = result.add(valueRemaining.longValue());
		result.isNegative = willBeNegative;
		return result;
	}

	public static InfiniteInteger littleEndian(long[] valueArray, boolean isNegative) {
		Long[] wrappedValues = new Long[valueArray.length];
		for(int i=0; i < valueArray.length; i++){wrappedValues[i] = Long.valueOf(valueArray[i]);}
		return littleEndian(Arrays.asList(wrappedValues).listIterator(), isNegative);
	}

	public static InfiniteInteger bigEndian(long[] valueArray, boolean isNegative) {
		Long[] wrappedValues = new Long[valueArray.length];
		for(int i=0; i < valueArray.length; i++){wrappedValues[i] = Long.valueOf(valueArray[i]);}
		return bigEndian(Arrays.asList(wrappedValues).listIterator(), isNegative);
	}

	public static InfiniteInteger littleEndian(Iterator<Long> valueIterator, boolean isNegative) {
		if(!valueIterator.hasNext()) return ZERO;
		InfiniteInteger result = InfiniteInteger.valueOf(valueIterator.next());
		result.isNegative = isNegative;
		DequeNode<Integer> cursor = result.magnitudeHead;
		if(cursor.getNext() == null) cursor = DequeNode.Factory.createNodeAfter(cursor, 0);
		while (valueIterator.hasNext())
		{
			long value = valueIterator.next();
			cursor = DequeNode.Factory.createNodeAfter(cursor, (int) value);
			value >>>= 32;
			cursor = DequeNode.Factory.createNodeAfter(cursor, (int) value);
		}
		while (cursor.getData().intValue() == 0)  //cursor is always at the last node
		{
			cursor = cursor.getPrev();
			if(cursor == null) return ZERO;  //if the last and only node was 0
			cursor.getNext().remove();
		}
		return result;
	}

	public static InfiniteInteger bigEndian(ListIterator<Long> valueIterator, boolean isNegative) {
		return littleEndian(DescendingListIterator.iterateBackwardsFromEnd(valueIterator), isNegative);
	}

	/**
	 * This method returns an infinite stream of all integers.
	 * NaN, +Infinity, and -Infinity will not be included in the stream.
	 * The stream's order is: 0, 1, -1, 2, -2, 3, -3, 4, -4...
	 *
	 * @return an infinite stream of all integers
	 */
	public static Stream<InfiniteInteger> streamAllIntegers() {
		return Stream.iterate(ZERO, (InfiniteInteger previous) -> {
				if(previous == ZERO) return InfiniteInteger.valueOf(1);
				if(previous.isNegative) return previous.abs().add(1);
				return previous.negate();
			});
	}

	@Override
	public int intValue() {
		if(!this.isFinite()) throw new ArithmeticException(this.toString()+" can't be even partially represented as an int.");
		int intValue = magnitudeHead.getData().intValue() & Integer.MAX_VALUE;  //drop the sign bit (can't use Math.abs because the nodes are unsigned)
		if(isNegative) return -intValue;
		return intValue;
	}

	@Override
	public float floatValue() {
		return (float) longValue();
	}

	@Override
	public double doubleValue() {
		return (double) longValue();
	}

	@Override
	public long longValue() {
		if(!this.isFinite()) throw new ArithmeticException(this.toString()+" can't be even partially represented as a long.");
		if(this == ZERO) return 0;

		long longValue = Integer.toUnsignedLong(magnitudeHead.getData().intValue());
		if (magnitudeHead.getNext() != null)
		{
			longValue += (Integer.toUnsignedLong(magnitudeHead.getNext().getData().intValue()) << 32);
		}
		longValue &= Long.MAX_VALUE;  //drop the sign bit (can't use Math.abs because the nodes are unsigned)
		if(isNegative) return -longValue;
		return longValue;
	}

	public long longValueExact() {
		if(!this.isFinite()) throw new ArithmeticException(this.toString()+" can't be represented as a long.");
		if(magnitudeHead.getNext() != null && magnitudeHead.getNext().getNext() != null)
			throw new ArithmeticException("This InfiniteInteger is too large to be represented as a long.");
			//if there are too many nodes then the number is too large
		if(magnitudeHead.getNext() != null && (magnitudeHead.getNext().getData().intValue() & Long.MIN_VALUE) != 0)
			throw new ArithmeticException("This InfiniteInteger is too large to be represented as a signed long.");
			//the & Min part checks that the most significant bit must be clear since it will be dropped to make the number signed
		return longValue();
	}

	public BigInteger bigIntegerValue() {
		//TODO: method stubs
		return null;
	}

	public BigInteger bigIntegerValueExact() {
		if(!this.isFinite()) throw new ArithmeticException(this.toString()+" can't be represented as a BigInteger.");
		if(this == ZERO) return BigInteger.ZERO;

		try {
			DequeNode<Integer> cursor = this.magnitudeHead;
			BigInteger result = BigInteger.valueOf(cursor.getData().intValue());
			cursor = cursor.getNext();
			while (cursor != null)
			{
				result = result.shiftLeft(32);
				result = result.or(BigInteger.valueOf(cursor.getData().intValue()));  //faster than add and will do the same thing
				cursor = cursor.getNext();
			}
			if(this.isNegative) result = result.negate();
			return result;
		} catch(Throwable t) {
			//ArithmeticException (from 1.8 overflow) or OutOfMemoryError etc
			//before 1.8 I assume it would throw ArrayIndexOutOfBoundsException
			//result.or will not throw but result.shiftLeft might
			throw new ArithmeticException("This InfiniteInteger is too large to be represented as a BigInteger.");
		}
	}

	public ListIterator<Integer> magnitudeIterator() {
		return new ReadOnlyListIterator<Integer>(new DequeNodeIterator.IndexAgnosticValueIterator<Integer>(magnitudeHead));
	}

	//exists to better represent that it can be any length
	public Stream<Integer> magnitudeStream() {
		return StreamSupport.stream(Spliterators.spliteratorUnknownSize(
				magnitudeIterator(),
                Spliterator.ORDERED | Spliterator.IMMUTABLE), false);
	}

	protected DequeNode<Integer> getMagnitudeTail() {
		//TODO: make a variable for magnitudeTail?
		DequeNode<Integer> tail = magnitudeHead;
		while(tail.getNext() != null) tail = tail.getNext();
		return tail;
	}

	public InfiniteInteger add(long value) {
		if(!this.isFinite() || value == 0) return this;
		if(this == ZERO) return InfiniteInteger.valueOf(value);
		if(value == Long.MIN_VALUE) return this.add(InfiniteInteger.valueOf(value));  //special case to avoid bug

		//delegations based on the sign of each
		if(!isNegative && value < 0) return this.subtract(Math.abs(value));
		if(isNegative && value > 0) return InfiniteInteger.valueOf(value).subtract(this.abs());
		if(isNegative && value < 0) return this.abs().add(Math.abs(value)).negate();
		//TODO: later consider making a mutable InfiniteInteger for speed (like above) that immutable would wrap

		//the rest is for if both positive
		long sum, valueRemaining = value;
		InfiniteInteger result = new InfiniteInteger(0);  //can't use ZERO because result will be modified
		DequeNode<Integer> returnCursor = result.magnitudeHead;
		DequeNode<Integer> thisCursor = this.magnitudeHead;
		int lowValue, highValue;
		while (thisCursor != null)
		{
			//turns out (true for signed and unsigned) max long > max int * max int. (2^64-1) > ((2^32-1)*(2^32-1))
			lowValue = (int) valueRemaining;
			highValue = (int) (valueRemaining >>> 32);
			sum = Integer.toUnsignedLong(thisCursor.getData().intValue()) + Integer.toUnsignedLong(lowValue);

			returnCursor.setData((int) sum);
			sum >>>= 32;

			valueRemaining = sum + Integer.toUnsignedLong(highValue);  //TODO: make a test that proves I need to use unsigned

			returnCursor = DequeNode.Factory.createNodeAfter(returnCursor, 0);
			thisCursor = thisCursor.getNext();
		}
		if (valueRemaining != 0)
		{
			//the addition's carry causes the return value to have more nodes
			lowValue = (int) valueRemaining;
			highValue = (int) (valueRemaining >>> 32);
			returnCursor.setData(lowValue);
			returnCursor = DequeNode.Factory.createNodeAfter(returnCursor, highValue);
		}
		if(returnCursor.getData().intValue() == 0) returnCursor.remove();  //remove the last node since it is leading 0s
		//do not use else. this can occur either way
		return result;
	}

	public InfiniteInteger add(BigInteger value) {
		return this.add(InfiniteInteger.valueOf(value));
	}

	public InfiniteInteger add(InfiniteInteger value) {
		if(!this.isFinite() || value == ZERO) return this;
		if(!value.isFinite() || this == ZERO) return value;

		//delegations based on the sign of each
		if(!isNegative && value.isNegative) return this.subtract(value.abs());
		if(isNegative && !value.isNegative) return value.subtract(this.abs());
		if(isNegative && value.isNegative) return this.abs().add(value.abs()).negate();

		//the rest is for if both positive
		long sum = 0;
		InfiniteInteger result = this.copy();
		DequeNode<Integer> returnCursor = result.magnitudeHead;
		ListIterator<Integer> valueMagIterator = value.magnitudeIterator();
		int lowSum, highSum;
		while (valueMagIterator.hasNext() || sum != 0)
		{
			//turns out (true for signed and unsigned) max long > max int * max int. (2^64-1) > ((2^32-1)*(2^32-1))
			lowSum = (int) sum;
			highSum = (int) (sum >>> 32);
			sum = Integer.toUnsignedLong(returnCursor.getData().intValue());
			if(valueMagIterator.hasNext()) sum += Integer.toUnsignedLong(valueMagIterator.next().intValue());
			sum += Integer.toUnsignedLong(lowSum);

			returnCursor.setData((int) sum);
			sum >>>= 32;
			sum += Integer.toUnsignedLong(highSum);

			if(returnCursor.getNext() == null) returnCursor = DequeNode.Factory.createNodeAfter(returnCursor, 0);
			else returnCursor = returnCursor.getNext();
		}
		if(returnCursor.getData().intValue() == 0) returnCursor.remove();  //remove the last node since it is leading 0s
		return result;
	}

	public InfiniteInteger subtract(long value) {
		if(!this.isFinite() || value == 0) return this;
		if(value == Long.MIN_VALUE) return this.add(InfiniteInteger.valueOf(value).abs());  //special case to avoid bug
		if(this == ZERO) return InfiniteInteger.valueOf(-value);

		//delegations based on the sign of each
		if(!isNegative && value < 0) return this.add(Math.abs(value));
		if(isNegative && value > 0) return this.abs().add(value).negate();
		if(isNegative && value < 0) return InfiniteInteger.valueOf(Math.abs(value)).subtract(this.abs());

		//the rest is for if both positive
		if(this.equals(value)) return ZERO;
		if(is(this, LESS_THAN, InfiniteInteger.valueOf(value))) return InfiniteInteger.valueOf(value).subtract(this).negate();

		//this is greater than value
		long difference, valueRemaining = value;
		InfiniteInteger result = new InfiniteInteger(0);  //can't use ZERO because result will be modified
		DequeNode<Integer> returnCursor = result.magnitudeHead;
		DequeNode<Integer> thisCursor = this.magnitudeHead;
		int lowValue, highValue;
		boolean borrow = false;
		while (thisCursor != null)
		{
			lowValue = (int) valueRemaining;
			highValue = (int) (valueRemaining >>> 32);
			difference = Integer.toUnsignedLong(thisCursor.getData().intValue()) - Integer.toUnsignedLong(lowValue);
				//Long.min is not possible so there's no bug to check
			borrow = (difference < 0);
			if(borrow) difference += Integer.toUnsignedLong((int) BitWiseUtil.HIGH_64) +1;  //add max unsigned int +1
				//this makes difference borrow
				//the +1 is here for the same reason that when borrowing in base 10 you add 10 instead of 9

			returnCursor.setData((int) difference);
			//assert((difference >>> 32) == 0);  //due to the borrowing above

			valueRemaining = Integer.toUnsignedLong(highValue);
			if(borrow) valueRemaining++;  //subtract 1 more

			returnCursor = DequeNode.Factory.createNodeAfter(returnCursor, 0);
			thisCursor = thisCursor.getNext();
		}
		//assert(valueRemaining == 0);  //because this > value
		//TODO: isn't there always a leading 0?
		if(returnCursor.getData().intValue() == 0){returnCursor = returnCursor.getPrev(); returnCursor.getNext().remove();}
			//remove the last node since it is leading 0s
			//assert(returnCursor != null)  //I already checked that this != value which is the only way for result == 0
		if(returnCursor.getData().intValue() == 0) returnCursor.remove();  //there will be 2 if the last node was borrowed down to 0

		return result;
	}

	public InfiniteInteger subtract(BigInteger value) {
		return subtract(InfiniteInteger.valueOf(value));
	}

	public InfiniteInteger subtract(InfiniteInteger value) {
		if(!this.isFinite() || value == ZERO) return this;
		if(this == ZERO || !value.isFinite()) return value.negate();

		//delegations based on the sign of each
		if(!this.isNegative && value.isNegative) return this.add(value.abs());
		if(this.isNegative && !value.isNegative) return this.abs().add(value).negate();
		if(this.isNegative && value.isNegative) return value.abs().subtract(this.abs());

		//the rest is for if both positive
		if(this.equals(value)) return ZERO;
		if(is(this, LESS_THAN, value)) return value.subtract(this).negate();

		//this is greater than value
		long difference = 0;
		InfiniteInteger result = this.copy();
		DequeNode<Integer> returnCursor = result.magnitudeHead;
		ListIterator<Integer> valueMagIterator = value.magnitudeIterator();
		int lowValue, highValue;
		byte borrow;
		while (valueMagIterator.hasNext() || difference != 0)
		{
			lowValue = (int) difference;
			highValue = (int) (difference >>> 32);
			difference = Integer.toUnsignedLong(returnCursor.getData().intValue());
			if(valueMagIterator.hasNext()) difference -= Integer.toUnsignedLong(valueMagIterator.next().intValue());
			difference -= Integer.toUnsignedLong(lowValue);
				//difference == Long.min won't cause a bug due to how borrow is programmed
			borrow = 0;
			while (difference < 0)  //can happen 0-2 times
			{
				borrow++;
				difference += Integer.toUnsignedLong((int) BitWiseUtil.HIGH_64) +1;  //add max unsigned int +1
				//this makes difference borrow
				//the +1 is here for the same reason that when borrowing in base 10 you add 10 instead of 9
			}

			returnCursor.setData((int) difference);
			//assert((difference >>> 32) == 0);  //due to the borrowing above

			difference = Integer.toUnsignedLong(highValue) + borrow;  //borrow subtracts more

			if(returnCursor.getNext() != null) returnCursor = returnCursor.getNext();
			//if returnCursor is at the end then the loop is done because this > value
		}
		if(returnCursor.getData().intValue() == 0){returnCursor = returnCursor.getPrev(); returnCursor.getNext().remove();}
			//remove the last node since it is leading 0s
			//assert(returnCursor != null)  //I already checked that this != value which is the only way for result == 0
		if(returnCursor.getData().intValue() == 0) returnCursor.remove();  //there will be 2 if the last node was borrowed down to 0

		return result;
	}

    public InfiniteInteger multiply(long value) {
		if(value == 0 && this.isInfinite()) return NaN;
    	if(!this.isFinite() || value == 1) return this;
		if(this == ZERO || value == 0) return ZERO;
    	if(value == -1) return this.negate();
    	if(this.equals(1)) return InfiniteInteger.valueOf(value);

		boolean resultIsNegative = (isNegative != value < 0);  //!= acts as xor
    	long valueRemaining = Math.abs(value);

		int lowValue = (int) valueRemaining;
		int highValue = (int) (valueRemaining >>> 32);

		InfiniteInteger result = thisMultiply(lowValue);
		if(highValue != 0) result = result.add(thisMultiply(highValue).shiftLeft(32));
    	result.isNegative = resultIsNegative;

		return result;
    }

    protected InfiniteInteger thisMultiply(int value) {
    	if(value == 0) return ZERO;
    	InfiniteInteger result = this.copy();
    	result.isNegative = false;  //faster than abs (which might not copy)
		DequeNode<Integer> resultCursor = result.magnitudeHead;
		long product;
		int carry = 0;
		boolean isHuge;
    	while (resultCursor != null)
		{
			//max unsigned int * max unsigned int < max unsigned long but will end up being negative which makes adding carry impossible
    		product = Integer.toUnsignedLong(resultCursor.getData().intValue());
    		product *= Integer.toUnsignedLong(value);
    		isHuge = product < 0;
    		product &= Long.MAX_VALUE;  //TODO: is Long.min possible?
    		product += Integer.toUnsignedLong(carry);
    		if(isHuge) product |= Long.MIN_VALUE;

			resultCursor.setData((int) product);
			product >>>= 32;
			carry = (int) product;

			resultCursor = resultCursor.getNext();
		}
    	resultCursor = result.getMagnitudeTail();
    	if(carry != 0) DequeNode.Factory.createNodeAfter(resultCursor, carry);
    	else if(resultCursor.getData().intValue() == 0) resultCursor.remove();
    	return result;
    }

    public InfiniteInteger multiply(BigInteger value) {
		return multiply(InfiniteInteger.valueOf(value));
    }

    public InfiniteInteger multiply(InfiniteInteger value) {
		if(value == ZERO && this.isInfinite()) return NaN;
		if(this == ZERO && value.isInfinite()) return NaN;
    	if(!this.isFinite() || value.equals(1)) return this;
    	if(!value.isFinite() || this.equals(1)) return value;
    	if(this == ZERO || value == ZERO) return ZERO;
    	if(value.equals(-1)) return this.negate();
    	if(this.equals(-1)) return value.negate();

    	/*
    	 * psudo code that BigInteger uses to multiply:
    	 * for each value node
    	 * {
    	 * 		long smallCarry = 0
    	 * 		for each node of this
    	 * 		{
    	 * 			long product = (this node * value node)
    	 * 			if(not 0) product += bigCarry node  //from a previous line
    	 * 			product += smallCarry
    	 * 			bigCarry node = product low
    	 * 			smallCarry = product high
    	 * 		}
    	 * 		result lowest node is now done = highest remaining smallCarry
    	 * }
    	 *
    	 * much like:
    	 *   23
    	 *  x15
    	 * ====
    	 *  115
    	 * +23
    	 * ====
    	 *  345
    	 *
    	 */
		boolean resultIsNegative = (isNegative != value.isNegative);  //!= acts as xor
		InfiniteInteger valueRemaining = value.abs();
    	InfiniteInteger result = ZERO;

		for (InfiniteInteger digit = ZERO; valueRemaining != ZERO; digit = digit.add(1))
		{
			InfiniteInteger product = thisMultiply(valueRemaining.magnitudeHead.getData().intValue());
			product = product.shiftLeft(digit.multiply(32));
			result = result.add(product);
			valueRemaining.shiftRight(valueRemaining);
		}
    	result.isNegative = resultIsNegative;
    	//TODO: make it not suck by using a cursor instead of shifting

		return result;
    }

    public IntegerQuotient<InfiniteInteger> divide(long val) {
		return divide(InfiniteInteger.valueOf(val));
    }

    public IntegerQuotient<InfiniteInteger> divide(BigInteger val) {
		return divide(InfiniteInteger.valueOf(val));
    }

    public IntegerQuotient<InfiniteInteger> divide(InfiniteInteger val) {
		// method stub
		return null;
    }

    //aka divideReturnWhole
    public InfiniteInteger divideDropRemainder(long val) {
    	//getMagnitudeTail()...?
		return divide(val).getWholeResult();
    }

    public InfiniteInteger divideDropRemainder(BigInteger val) {
		return divide(val).getWholeResult();
    }

    public InfiniteInteger divideDropRemainder(InfiniteInteger val) {
		return divide(val).getWholeResult();
    }

    //aka remainder, divideDropWhole, divideReturnRemainder
    public InfiniteInteger mod(long val) {
    	//longVal % val
		return divide(val).getRemainder();
    }

    public InfiniteInteger mod(BigInteger val) {
		return divide(val).getRemainder();
    }

    public InfiniteInteger mod(InfiniteInteger val) {
		return divide(val).getRemainder();
    }

    public InfiniteInteger pow(long val) {
		return pow(InfiniteInteger.valueOf(val));
    }

    public InfiniteInteger pow(BigInteger val) {
		return pow(InfiniteInteger.valueOf(val));
    }

    public InfiniteInteger pow(InfiniteInteger val) {
		// method stub
    	//call mutliply in a loop for now
		return null;
    }

    //this^this. exists mostly as a testimony that this class really can hold any integer
    public InfiniteInteger selfPower() {
		return pow(this);
    }

    //why not. same reason as self power
    public InfiniteInteger factorial() {
		if(this.isNegative || this == NaN) return NaN;  //factorial is not defined for negative numbers
		if(this == POSITIVE_INFINITITY) return this;  //-Infinity is covered above
		if(this == ZERO || this.equals(1)) return InfiniteInteger.valueOf(1);

		InfiniteInteger result = this.copy();
		InfiniteInteger valueRemaining = result.subtract(1);
    	while (valueRemaining != ZERO)
    	{
    		result = result.multiply(valueRemaining);
    		valueRemaining = valueRemaining.subtract(1);
    		//it's faster to let multiply(1) fast path then it is to compare valueRemaining to 1
    		//since multiply will always compare the parameter to 1 anyway
    	}
		return result;
    }

    public InfiniteInteger abs() {
    	if(!isNegative || isNaN()) return this;  //includes this == 0 and +Infinity
    	if(this == NEGATIVE_INFINITITY) return POSITIVE_INFINITITY;
    	InfiniteInteger result = copy();
    	result.isNegative = false;
    	return result;
    }

    public InfiniteInteger negate() {
    	if(isNaN() || this == ZERO) return this;
    	if(this == NEGATIVE_INFINITITY) return POSITIVE_INFINITITY;
    	if(this == POSITIVE_INFINITITY) return NEGATIVE_INFINITITY;
    	InfiniteInteger result = copy();
    	result.isNegative = !isNegative;
    	return result;
    }

    /**
     * @return -1, 0 or 1 as the value of this number is negative, zero or
     *         positive respectively. NaN returns 0.
     */
    public byte signum() {
    	if(isNegative) return -1;
    	if(this == ZERO || this == NaN) return 0;
        return 1;
    }

    public InfiniteInteger shiftLeft(long n) {
		return shiftLeft(InfiniteInteger.valueOf(n));
    }

    public InfiniteInteger shiftLeft(BigInteger n) {
		return shiftLeft(InfiniteInteger.valueOf(n));
    }

    //this*2^x
    public InfiniteInteger shiftLeft(InfiniteInteger value) {
		if(value == ZERO || !this.isFinite()) return this;
		if(value.isNegative) return this.shiftRight(value.abs());

		InfiniteInteger result = this.copy();
		InfiniteInteger valueRemaining = value;
		while (isComparisonResult(valueRemaining.compareTo(32), GREATER_THAN_OR_EQUAL_TO))
		{
			result.magnitudeHead = DequeNode.Factory.createNodeBefore(0, result.magnitudeHead);
			valueRemaining = valueRemaining.subtract(32);
		}

		final int smallValueRemaining = valueRemaining.intValue();
		if (smallValueRemaining != 0)
		{
			DequeNode<Integer> returnCursor = result.getMagnitudeTail();

			while (returnCursor != null)
			{
				int overflow = BitWiseUtil.getHighestNBits(returnCursor.getData().intValue(), smallValueRemaining);
					//overflow contains what would fall off when shifting left
				overflow >>>= (32 - smallValueRemaining);
					//shift overflow right so that it appears in the least significant place of the following node
				if(overflow != 0 && returnCursor.getNext() == null) DequeNode.Factory.createNodeAfter(returnCursor, overflow);
				else if(overflow != 0) returnCursor.getNext().setData(returnCursor.getNext().getData().intValue() | overflow);

				returnCursor.setData(returnCursor.getData().intValue() << smallValueRemaining);
				returnCursor = returnCursor.getPrev();
			}
		}

		return result;
    }

    public InfiniteInteger shiftRight(long n) {
		return shiftRight(InfiniteInteger.valueOf(n));
    }

    public InfiniteInteger shiftRight(BigInteger n) {
		return shiftRight(InfiniteInteger.valueOf(n));
    }

    //this/2^x
    public InfiniteInteger shiftRight(InfiniteInteger value) {
		if(value == ZERO || !this.isFinite()) return this;
		if(value.isNegative) return this.shiftLeft(value.abs());

		InfiniteInteger result = this.copy();
		InfiniteInteger valueRemaining = value;
		while (isComparisonResult(valueRemaining.compareTo(32), GREATER_THAN_OR_EQUAL_TO))
		{
			result.magnitudeHead = result.magnitudeHead.getNext();
			if(result.magnitudeHead == null) return ZERO;
			result.magnitudeHead.getPrev().remove();
			valueRemaining = valueRemaining.subtract(32);
		}

		final int smallValueRemaining = valueRemaining.intValue();
		if (smallValueRemaining != 0)
		{
			DequeNode<Integer> returnCursor = result.magnitudeHead;

			while (returnCursor.getNext() != null)
			{
				returnCursor.setData(returnCursor.getData().intValue() >>> smallValueRemaining);
				int underflow = (int) BitWiseUtil.getLowestNBits(returnCursor.getNext().getData().intValue(), smallValueRemaining);
					//underflow contains what would fall off when shifting right
				underflow <<= (32 - smallValueRemaining);
					//shift underflow left so that it appears in the most significant place of the previous node
				if(underflow != 0) returnCursor.setData(returnCursor.getData().intValue() | underflow);
				returnCursor = returnCursor.getNext();
			}
			//last node simply shifts
			returnCursor.setData(returnCursor.getData().intValue() >>> smallValueRemaining);
		}
		if(result.magnitudeHead.getNext() == null && result.magnitudeHead.getData().intValue() == 0) return ZERO;

		return result;
    }

    //TODO: add min/max. maybe static (InfInt, InfInt) only?
    //big int also has bitwise operations. gcd. and weird methods

    public boolean isNaN(){return this == NaN;}
	public boolean isInfinite(){return (this == POSITIVE_INFINITITY || this == NEGATIVE_INFINITITY);}
	public boolean isFinite(){return (!this.isNaN() && !this.isInfinite());}
	public void signalNaN(){if(isNaN()) throw new ArithmeticException("Not a number.");}

	//these can't be symmetric
	public boolean equals(int value) {
		if(!this.isFinite()) return false;
		if(magnitudeHead.getNext() != null) return false;  //this is too large to fit into an int
		int thisIntValue = this.intValue();
		if(magnitudeHead.getData().intValue() != thisIntValue) return false;  //this is too large to fit into a signed int
		return (thisIntValue == value);
	}

	public boolean equals(Integer value) {
		if(value == null) return false;
		return this.equals(value.intValue());
	}

	public boolean equals(long value) {
		if(!this.isFinite()) return false;

		if(magnitudeHead.getNext() != null && magnitudeHead.getNext().getNext() != null)
			return false;  //can't fit into long
		if(magnitudeHead.getNext() != null && (magnitudeHead.getNext().getData().intValue() & Long.MIN_VALUE) != 0)
			return false;  //can't fit into signed long

		return (value == this.longValue());
	}

	public boolean equals(Long value) {
		if(value == null) return false;
		return this.equals(value.longValue());
	}

	//asymmetric warning
	@Override
	public boolean equals(Object obj) {
		if(obj == null) return false;
		if(obj instanceof InfiniteInteger) return this.equals((InfiniteInteger) obj);
		if(!this.isFinite()) return false;
		if(obj instanceof BigInteger) return this.equals(InfiniteInteger.valueOf((BigInteger) obj));
		if(obj instanceof Number) return this.equals(((Number) obj).longValue());
		return false;
	}

	public boolean equals(InfiniteInteger other) {
		if(this == other) return true;
		//these are singletons. if not the same object then it's not equal
		if(this == ZERO || !this.isFinite() || other == ZERO || !other.isFinite()) return false;
		if(isNegative != other.isNegative) return false;
		DequeNode<Integer> thisCursor = this.magnitudeHead;
		DequeNode<Integer> otherCursor = other.magnitudeHead;
		while (thisCursor != null && otherCursor != null)
		{
			if(!Objects.equals(thisCursor.getData(), otherCursor.getData())) return false;
			thisCursor = thisCursor.getNext();
			otherCursor = otherCursor.getNext();
		}
		return (thisCursor == otherCursor);  //they must both be null (at end)
	}

	//collisions are, in theory, likely
	@Override
	public int hashCode() {
		DequeNode<Integer> cursor = magnitudeHead;
		int hash = Boolean.hashCode(isNegative);
		while (cursor != null)
		{
			hash ^= cursor.getData().intValue();
			cursor = cursor.getNext();
		}
		return hash;
	}

	@Override
	public String toString() {
		if(this == POSITIVE_INFINITITY) return "+Infinity";  //it doesn't seem like \u221E works
		if(this == NEGATIVE_INFINITITY) return "-Infinity";
		if(this == NaN) return "NaN";
		if(this == ZERO) return "0";
		String returnValue = "+ ";
		if(isNegative) returnValue = "- ";
		//method stub
		//BigInteger > string max
		//BigInteger.toString(any) doesn't check range and will just crash
		//BigInteger.toString(not small) is recursive and will run out of RAM
		//instead of messing with all that I think I'll just return the base 10 string if possible or "> 2^max+" otherwise
		//unfortunately this stores in base 2 so I don't know how to display it in base 10
		//return "2^?";

		//string for debugging:
		StringBuilder stringBuilder = new StringBuilder(returnValue);
		for (DequeNode<Integer> cursor = magnitudeHead; cursor != null; cursor = cursor.getNext())
		{
			stringBuilder.append(Integer.toHexString(cursor.getData().intValue()).toUpperCase());
			stringBuilder.append(", ");
		}
		return stringBuilder.toString();
	}

	public void toFile(File writeToHere) {
		// method stub it can always fit
	}
    //I previously implemented writeObject but there doesn't seem to be any way to implement readObject
    //since I don't know how many nodes there are, therefore I deleted writeObject and trust the JVM to serialize
	//TODO: possible to serialize by putting a long for count of following nodes that exist and repeat
	//first test to see if default serialize works

	//javadoc the ones that will not be copied and that being immutable is not all that useful to the outside
	@Override
	public InfiniteInteger copy() {
		if(!this.isFinite() || this == ZERO) return this;
		InfiniteInteger returnValue = new InfiniteInteger(0);  //can't use ZERO because returnValue will be modified
		returnValue.isNegative = isNegative;
		DequeNode<Integer> returnCursor = returnValue.magnitudeHead;
		DequeNode<Integer> thisCursor = this.magnitudeHead;

		returnCursor.setData(thisCursor.getData());  //must be outside the loop since the first node already exists
		thisCursor = thisCursor.getNext();
		while (thisCursor != null)
		{
			returnCursor = DequeNode.Factory.createNodeAfter(returnCursor, thisCursor.getData());
			thisCursor = thisCursor.getNext();
		}
		return returnValue;
	}

	//natural order. but 0 < NaN < 1
	//&plusmn;&infin; is as expected
	@Override
	public int compareTo(InfiniteInteger other) {
		if(this == other) return THIS_EQUAL;
		if(this == POSITIVE_INFINITITY || other == NEGATIVE_INFINITITY) return THIS_GREATER;
		if(this == NEGATIVE_INFINITITY || other == POSITIVE_INFINITITY) return THIS_LESSER;

		if (this == NaN)
		{
			if(other == ZERO || other.isNegative) return THIS_GREATER;
			return THIS_LESSER;  //since other != NaN
		}
		if (other == NaN)
		{
			if(this == ZERO || this.isNegative) return THIS_LESSER;
			return THIS_GREATER;  //since this != NaN
		}

		if(isNegative && !other.isNegative) return THIS_LESSER;
		if(!isNegative && other.isNegative) return THIS_GREATER;  //also covers if this == ZERO
		if(this == ZERO && !other.isNegative) return THIS_LESSER;  //since this != other

		//at this point: they are not the same object, they have the same sign, they are not special values.
		//since the lengths can be any integer I first need to compare lengths

		DequeNode<Integer> otherCursor = other.magnitudeHead;
		DequeNode<Integer> thisCursor = this.magnitudeHead;
		//this loop will not execute if they both have 1 node
		//which is correct since they have equal length and are both pointing to last node
		while (thisCursor.getNext() != null || otherCursor.getNext() != null)
		{
			if (thisCursor.getNext() != null && otherCursor.getNext() != null)
			{
				thisCursor = thisCursor.getNext();
				otherCursor = otherCursor.getNext();
			}
			else if(thisCursor.getNext() != null) return THIS_GREATER;
			else return THIS_LESSER;
		}

		//they have the same number of nodes and both cursors are pointing to the most significant (last) node
		int thisData, otherData;
		while (thisCursor != null)
		{
			thisData = thisCursor.getData().intValue();
			otherData = otherCursor.getData().intValue();
			if(thisData != otherData) return Integer.compareUnsigned(thisData, otherData);
			thisCursor = thisCursor.getPrev();
			otherCursor = otherCursor.getPrev();
		}

		//same length and all nodes have the same data
		return THIS_EQUAL;
	}

	//even though it can't be sorted like this
	public int compareTo(BigInteger other) {
		return this.compareTo(InfiniteInteger.valueOf(other));
	}

	public int compareTo(long other) {
		return this.compareTo(InfiniteInteger.valueOf(other));
	}

}
