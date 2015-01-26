package com.github.SkySpiral7.Java;

import java.io.File;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.Objects;
import java.util.stream.Stream;

import com.github.SkySpiral7.Java.iterators.DequeNodeIterator;
import com.github.SkySpiral7.Java.iterators.DescendingListIterator;
import com.github.SkySpiral7.Java.iterators.ReadOnlyListIterator;
import com.github.SkySpiral7.Java.pojo.DequeNode;
import com.github.SkySpiral7.Java.pojo.IntegerQuotient;

//aka InfinInt
//maxes: int[] 2^(32 * (2^31-1))-1 long[] 2^(64 * (2^31-1))-1
//string (base 10): 10^(2^31-1) which is much smaller; big int: ?
public class InfiniteInteger extends Number implements Copyable<InfiniteInteger>, Comparable<InfiniteInteger> {
	private static final long serialVersionUID = 1L;

	public static final InfiniteInteger ZERO = new InfiniteInteger(0);
	public static final InfiniteInteger NOT_A_NUMBER = new InfiniteInteger(false);
	public static final InfiniteInteger POSITIVE_INFINITITY = new InfiniteInteger(true);
	public static final InfiniteInteger NEGATIVE_INFINITITY = new InfiniteInteger(false);

	//protected static final BigInteger bigIntegerMaxUnsignedLong = BigInteger.valueOf(Long.MAX_VALUE).shiftLeft(1).add(BigInteger.ONE);
	protected static final BigInteger bigIntegerMaxLong = BigInteger.valueOf(Long.MAX_VALUE);
	//private static InfiniteInteger maxBigInteger;
	/**This is also max unsigned integer*/
	public static final int LOW_MASK_64 = 0xFFFF_FFFF;
	//public static final long HIGH_MASK_64 = 0xFFFF_FFFF_0000_0000L;

	/**
	 * Little endian: the first node is the least significant.
	 */
	protected DequeNode<Integer> magnitudeHead;
	protected boolean isNegative;

	/**
	 * This constructor is used to make special constants.
	 * My making the head null it does something the other constructor can't.
	 * @param isNegative used for positive and negative infinity. Meaningless to not a number.
	 */
	protected InfiniteInteger(boolean isNegative){magnitudeHead = null; this.isNegative = isNegative;}
	protected InfiniteInteger(long value) {
		isNegative = (value < 0);
		value = Math.abs(value);
		magnitudeHead = DequeNode.Factory.createStandAloneNode((int) (value & LOW_MASK_64));
		if(value > LOW_MASK_64) DequeNode.Factory.createNodeAfter(magnitudeHead, ((int) (value >>> 32)));
	}

	public static InfiniteInteger valueOf(long value) {
		if(value == 0) return ZERO;
		return new InfiniteInteger(value);
	}
	public static InfiniteInteger valueOf(BigInteger value) {
		if(value.compareTo(bigIntegerMaxLong) != 1) return valueOf(value.longValue());
		return ZERO.add(value);
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

	public static InfiniteInteger littleEndian(Iterator<Long> valueArray, boolean isNegative) {
		//TODO: method stub
		return null;
	}

	public static InfiniteInteger bigEndian(ListIterator<Long> valueArray, boolean isNegative) {
		return littleEndian(DescendingListIterator.iterateBackwards(valueArray), isNegative);
	}

	/**
	 * This method returns an infinite stream of all integers.
	 * NaN, +Infinity, and -Infinity will not be included in the stream.
	 * The stream's order is: 0, 1, -1, 2, -2, 3, -3, 4, -4...
	 * 
	 * @return a stream of all integers
	 */
	public Stream<InfiniteInteger> streamAllIntegers() {
		return Stream.iterate(ZERO, (InfiniteInteger previous) -> {
				if(previous == ZERO) return new InfiniteInteger(1);
				if(previous.isNegative) return previous.abs().add(1);
				return previous.negate();
			});
	}

	@Override
	public int intValue() {
		int returnValue = Math.abs(magnitudeHead.getData().intValue());
		if(isNegative) return -returnValue;
		return returnValue;
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
		// TODO method stub
		return 0;
	}

	public long longValueExact() {
		// TODO method stub
		return 0;
	}

	public BigInteger bigIntegerValue() {
		// TODO method stub
		return null;
	}

	public BigInteger bigIntegerValueExact() {
		// TODO method stub
		return null;
	}

	public ListIterator<Integer> magnitudeIterator() {
		return new ReadOnlyListIterator<Integer>(new DequeNodeIterator.IndexAgnosticValueIterator<Integer>(magnitudeHead));
	}

	public InfiniteInteger add(long value) {
		//TODO: currently assumes all positive
		if(!this.isFinite() || value == 0) return this;
		if(this == ZERO) return new InfiniteInteger(value);
		long sum, valueRemaining = value;
		InfiniteInteger returnValue = new InfiniteInteger(0);  //can't use ZERO because returnValue will be modified
		DequeNode<Integer> returnCursor = returnValue.magnitudeHead;
		DequeNode<Integer> thisCursor = this.magnitudeHead;
		int lowValue, highValue;
		while (thisCursor != null)
		{
			/*
			int lowValue = (int) (valueRemaining & LOW_MASK_64);
			int lowData = (int) (thisCursor.getData() & LOW_MASK_64);
			long lowResult = lowData+lowValue;
			int lowlowResult = (int) (lowResult & LOW_MASK_64);
			int highlowResult = (int) (lowResult >>> 32);

			int highValue = (int) (valueRemaining >>> 32);
			int highData = (int) (thisCursor.getData() >>> 32);
			long highResult = highData+highValue+highlowResult;
			int lowhighResult = (int) (highResult & LOW_MASK_64);
			valueRemaining = (int) (highResult >>> 32);

			long result = (lowhighResult << 32) + lowlowResult;
			*/

			/*long spaceAvailable = (Integer.toUnsignedLong(Integer.MAX_UNSIGNED_VALUE) - Integer.toUnsignedLong(thisCursor.getData()));
			if(spaceAvailable >= valueRemaining){
				returnCursor.setData((int) (Integer.toUnsignedLong(thisCursor.getData())+Integer.toUnsignedLong((int) valueRemaining)));
				valueRemaining = 0;}
			returnCursor = DequeNode.Factory.createNodeAfter(returnCursor, 0);
			thisCursor = thisCursor.getNext();*/

			//turns out (true for signed and unsigned) max long > max int * max int. (2^64-1) > ((2^32-1)*(2^32-1))
			if (valueRemaining != 0)
			{
				lowValue = ((int) (valueRemaining & LOW_MASK_64));
				highValue = (int) (valueRemaining >>> 32);
				sum = Integer.toUnsignedLong(thisCursor.getData()) + lowValue;

				//TODO: test: is &low the same as (int)?
				//TODO: confirm: is >>> 32 the number I want
				returnCursor.setData((int) (sum & LOW_MASK_64));
				sum >>>=32;

				valueRemaining = sum + highValue;
			}

			returnCursor = DequeNode.Factory.createNodeAfter(returnCursor, 0);
			thisCursor = thisCursor.getNext();

			if (valueRemaining != 0 && thisCursor != null)
			{
				lowValue = ((int) (valueRemaining & LOW_MASK_64));
				highValue = (int) (valueRemaining >>> 32);
				sum = Integer.toUnsignedLong(thisCursor.getData()) + lowValue;

				returnCursor.setData((int) (sum & LOW_MASK_64));
				sum >>>=32;

				valueRemaining = sum + highValue;
				returnCursor = DequeNode.Factory.createNodeAfter(returnCursor, 0);
				thisCursor = thisCursor.getNext();
			}

		}
		if (valueRemaining != 0)
		{
			lowValue = ((int) (valueRemaining & LOW_MASK_64));
			highValue = (int) (valueRemaining >>> 32);
			returnCursor.setData(lowValue);
			returnCursor = DequeNode.Factory.createNodeAfter(returnCursor, highValue);
		}
		if(returnCursor.getData().intValue() == 0) returnCursor.remove();  //remove the last one since has only leading 0s
		//do not use else. this can occur either way
		return returnValue;
	}

	public InfiniteInteger add(BigInteger value) {
		//TODO: currently assumes all positive
		if(!this.isFinite() || value.compareTo(BigInteger.ZERO) == 0) return this;
		if(value.compareTo(bigIntegerMaxLong) != 1) return add(value.longValue());
		InfiniteInteger returnValue = this;
		BigInteger valueRemaining = value;
		while (valueRemaining.compareTo(bigIntegerMaxLong) == 1)
		{
			returnValue = returnValue.add(Long.MAX_VALUE);
			valueRemaining = valueRemaining.subtract(bigIntegerMaxLong);
		}
		returnValue = returnValue.add(valueRemaining.longValue());
		return returnValue;
	}

	public InfiniteInteger add(InfiniteInteger value) {
		//TODO: currently assumes all positive
		if(!this.isFinite() || value == ZERO) return this;
		if(!value.isFinite() || this == ZERO) return value;
		// TODO method stub
		return null;
	}

	public InfiniteInteger subtract(long value) {
		// TODO method stub
		return null;
	}

	public InfiniteInteger subtract(BigInteger value) {
		return subtract(InfiniteInteger.valueOf(value));
	}

	public InfiniteInteger subtract(InfiniteInteger value) {
		// TODO method stub
		return null;
	}

    public InfiniteInteger multiply(long value) {
		// TODO method stub
		return null;
    }

    public InfiniteInteger multiply(BigInteger value) {
		return multiply(InfiniteInteger.valueOf(value));
    }

    public InfiniteInteger multiply(InfiniteInteger value) {
		// TODO method stub
		return null;
    }

    public IntegerQuotient<InfiniteInteger> divide(long val) {
		// TODO method stub
		return null;
    }

    public IntegerQuotient<InfiniteInteger> divide(BigInteger val) {
		return divide(InfiniteInteger.valueOf(val));
    }

    public IntegerQuotient<InfiniteInteger> divide(InfiniteInteger val) {
		// TODO method stub
		return null;
    }

    public InfiniteInteger divideDropRemainder(long val) {
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
		return divide(val).getRemainder();
    }

    public InfiniteInteger mod(BigInteger val) {
		return divide(val).getRemainder();
    }

    public InfiniteInteger mod(InfiniteInteger val) {
		return divide(val).getRemainder();
    }

    public InfiniteInteger pow(long val) {
		// TODO method stub
		return null;
    }

    public InfiniteInteger pow(BigInteger val) {
		return pow(InfiniteInteger.valueOf(val));
    }

    public InfiniteInteger pow(InfiniteInteger val) {
		// TODO method stub
		return null;
    }

    //this^this
    public InfiniteInteger selfPower() {
		return pow(this);
    }

    public InfiniteInteger abs() {
    	if(!isNegative || isNaN()) return this;  //includes this == 0 and +Infinity
    	if(this == NEGATIVE_INFINITITY) return POSITIVE_INFINITITY;
    	InfiniteInteger returnValue = copy();
    	returnValue.isNegative = false;
    	return returnValue;
    }

    public InfiniteInteger negate() {
    	if(isNaN() || this == ZERO) return this;
    	if(this == NEGATIVE_INFINITITY) return POSITIVE_INFINITITY;
    	if(this == POSITIVE_INFINITITY) return NEGATIVE_INFINITITY;
    	InfiniteInteger returnValue = copy();
    	returnValue.isNegative = !isNegative;
    	return returnValue;
    }

    /**
     * @return -1, 0 or 1 as the value of this number is negative, zero or
     *         positive respectively. NaN returns 0.
     */
    public byte signum() {
    	if(isNegative) return -1;
    	if(this == ZERO || this == NOT_A_NUMBER) return 0;
        return 1;
    }

    public InfiniteInteger shiftLeft(long n) {
		// TODO method stub
		return null;
    }

    public InfiniteInteger shiftLeft(BigInteger n) {
		return shiftLeft(InfiniteInteger.valueOf(n));
    }

    public InfiniteInteger shiftLeft(InfiniteInteger n) {
		// TODO method stub
		return null;
    }

    public InfiniteInteger shiftRight(long n) {
		// TODO method stub
		return null;
    }

    public InfiniteInteger shiftRight(BigInteger n) {
		return shiftRight(InfiniteInteger.valueOf(n));
    }

    public InfiniteInteger shiftRight(InfiniteInteger n) {
		// TODO method stub
		return null;
    }

    //TODO: add min/max. maybe static (InfInt, InfInt) only?
    //big int also has bitwise operations. gcd. and weird methods

    public boolean isNaN(){return this == NOT_A_NUMBER;}
	public static boolean isNaN(InfiniteInteger num){return num.isNaN();}
	public boolean isInfinite(){return (this == POSITIVE_INFINITITY || this == NEGATIVE_INFINITITY);}
	public static boolean isInfinite(InfiniteInteger num){return num.isInfinite();}
	public boolean isFinite(){return (!this.isNaN() && !this.isInfinite());}
	public static boolean isFinite(InfiniteInteger num){return (!num.isNaN() && !num.isInfinite());}

	@Override
	public boolean equals(Object obj) {
		if(this == obj) return true;
		if(this == ZERO || this == POSITIVE_INFINITITY || this == NEGATIVE_INFINITITY || this == NOT_A_NUMBER) return false;
		//these are singletons. if not the same object then it's not equal
		if(!(obj instanceof InfiniteInteger)) return false;  //includes null check and children
		InfiniteInteger other = (InfiniteInteger) obj;
		if(other == ZERO || other == POSITIVE_INFINITITY || other == NEGATIVE_INFINITITY || other == NOT_A_NUMBER) return false;
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
			hash ^= cursor.getData();
			cursor = cursor.getNext();
		}
		return hash;
	}

	@Override
	public String toString() {
		//TODO method stub
		//BigInteger > string max
		//BigInteger.toString(any) doesn't check range and will just crash
		//BigInteger.toString(not small) is recursive and will run out of RAM
		//instead of messing with all that I think I'll just return the base 10 string if possible or "> 2^max+" otherwise
		//unfortunately this stores in base 2 so I don't know how to display it in base 10
		return "2^?";
	}

	public void toFile(File writeToHere) {
		// TODO method stub it can always fit
	}

	//javadoc the ones that will not be copied and that being immutable is not all that usefull to the outside
	@Override
	public InfiniteInteger copy() {
		if(!this.isFinite() || this == ZERO) return this;
		InfiniteInteger returnValue = new InfiniteInteger(0);  //can't use ZERO because returnValue will be modified
		returnValue.isNegative = isNegative;
		DequeNode<Integer> returnCursor = returnValue.magnitudeHead;
		DequeNode<Integer> thisCursor = this.magnitudeHead;

		returnCursor.setData(thisCursor.getData());
		thisCursor = thisCursor.getNext();
		while (thisCursor != null)
		{
			returnCursor = DequeNode.Factory.createNodeAfter(returnCursor, thisCursor.getData());
			thisCursor = thisCursor.getNext();
		}
		return returnValue;
	}

	//natural order. but 0 < NaN < 1
	@Override
	public int compareTo(InfiniteInteger other) {
		byte thisSmaller = -1, thisBigger = 1, sameValue = 0;

		if(this == other) return sameValue;
		if(this == POSITIVE_INFINITITY || other == NEGATIVE_INFINITITY) return thisBigger;
		if(this == NEGATIVE_INFINITITY || other == POSITIVE_INFINITITY) return thisSmaller;

		if (this == NOT_A_NUMBER)
		{
			if(other == ZERO || other.isNegative) return thisBigger;
			return thisSmaller;  //since other != NaN
		}
		if (other == NOT_A_NUMBER)
		{
			if(this == ZERO || this.isNegative) return thisSmaller;
			return thisBigger;  //since this != NaN
		}

		if(isNegative && !other.isNegative) return thisSmaller;
		if(!isNegative && other.isNegative) return thisBigger;

		//at this point: they are not the same object, they have the same sign, they are not special values.
		//since the lengths can be any integer I first need to compare lengths

		DequeNode<Integer> otherCursor = other.magnitudeHead;
		DequeNode<Integer> thisCursor = this.magnitudeHead;
		while (thisCursor.getNext() != null || otherCursor.getNext() != null)
		{
			if (thisCursor.getNext() != null && otherCursor.getNext() != null)
			{
				thisCursor = thisCursor.getNext();
				otherCursor = otherCursor.getNext();
			}
			else if(thisCursor.getNext() != null) return thisBigger;
			else return thisSmaller;
		}

		//they have the same number of nodes and both cursors are pointing to the most significant (last) node
		Integer thisData, otherData;
		while (thisCursor.getPrev() != null)
		{
			thisData = thisCursor.getData();
			otherData = otherCursor.getData();
			if(thisData.intValue() != otherData.intValue()) return thisData.compareTo(otherData);
			thisCursor = thisCursor.getPrev();
			otherCursor = otherCursor.getPrev();
		}

		//same length and all nodes have the same data
		return sameValue;
	}

	//even though it can't be sorted like this
	public int compareTo(BigInteger other) {
		return this.compareTo(valueOf(other));
	}

	public int compareTo(long other) {
		return this.compareTo(valueOf(other));
	}

}