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

	/**This constant represents 0 and it is the only InfiniteInteger that can be 0 (singleton).
	 * Therefore it is safe to use pointer equality for comparison: <code>if(infiniteIntegerVar == InfiniteInteger.ZERO)</code>*/
	public static final InfiniteInteger ZERO = new InfiniteInteger(0);
	/**Common abbreviation for "not a number". This constant is the result of invalid math such as 0/0.*/
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
		boolean willBeNegative = (value.signum() == -1);
		BigInteger valueRemaining = value.abs();

		final BigInteger bigIntegerMaxLong = BigInteger.valueOf(Long.MAX_VALUE);
		if(valueRemaining.compareTo(bigIntegerMaxLong) != 1) return InfiniteInteger.valueOf(value.longValue());
		//if abs is less than or equal to max long. ie if it fits in a long then delegate

		InfiniteInteger returnValue = InfiniteInteger.ZERO;
		if(value.compareTo(BigInteger.ZERO) == 0) return returnValue;

		while (valueRemaining.compareTo(bigIntegerMaxLong) == 1)  //while larger than
		{
			returnValue = returnValue.add(Long.MAX_VALUE);
			valueRemaining = valueRemaining.subtract(bigIntegerMaxLong);
		}
		returnValue = returnValue.add(valueRemaining.longValue());
		returnValue.isNegative = willBeNegative;
		return returnValue;
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
		//TODO: method stubs
		return null;
	}

	public static InfiniteInteger bigEndian(ListIterator<Long> valueArray, boolean isNegative) {
		return littleEndian(DescendingListIterator.iterateBackwardsFromEnd(valueArray), isNegative);
	}

	/**
	 * This method returns an infinite stream of all integers.
	 * NaN, +Infinity, and -Infinity will not be included in the stream.
	 * The stream's order is: 0, 1, -1, 2, -2, 3, -3, 4, -4...
	 * 
	 * @return a stream of all integers
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

		long longValue = Integer.toUnsignedLong(magnitudeHead.getData());
		if (magnitudeHead.getNext() != null)
		{
			longValue += (Integer.toUnsignedLong(magnitudeHead.getNext().getData()) << 32);
		}
		longValue &= Long.MAX_VALUE;  //drop the sign bit (can't use Math.abs because the nodes are unsigned)
		if(isNegative) return -longValue;
		return longValue;
	}

	public long longValueExact() {
		if(!this.isFinite()) throw new ArithmeticException(this.toString()+" can't be represented as a long.");
		if(magnitudeHead.getNext() != null && magnitudeHead.getNext().getNext() != null)
			throw new ArithmeticException("This InfiniteInteger can't be represented as a long.");
			//if there are too many nodes then the number is too large
		if(magnitudeHead.getNext() != null && (magnitudeHead.getNext().getData().intValue() & Long.MIN_VALUE) != 0)
			throw new ArithmeticException("This InfiniteInteger can't be represented as a signed long.");
			//the & Min part checks that the most significant bit must be clear since it will be dropped to make the number signed
		return longValue();
	}

	public BigInteger bigIntegerValue() {
		// method stub
		return null;
	}

	public BigInteger bigIntegerValueExact() {
		// method stub
		return null;
	}

	public ListIterator<Integer> magnitudeIterator() {
		return new ReadOnlyListIterator<Integer>(new DequeNodeIterator.IndexAgnosticValueIterator<Integer>(magnitudeHead));
	}

	protected DequeNode<Integer> getMagnitudeTail() {
		//TODO: make a variable for magnitudeTail
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
		InfiniteInteger returnValue = new InfiniteInteger(0);  //can't use ZERO because returnValue will be modified
		DequeNode<Integer> returnCursor = returnValue.magnitudeHead;
		DequeNode<Integer> thisCursor = this.magnitudeHead;
		int lowValue, highValue;
		while (thisCursor != null)
		{
			//turns out (true for signed and unsigned) max long > max int * max int. (2^64-1) > ((2^32-1)*(2^32-1))
			lowValue = (int) valueRemaining;
			highValue = (int) (valueRemaining >>> 32);
			sum = Integer.toUnsignedLong(thisCursor.getData()) + Integer.toUnsignedLong(lowValue);

			returnCursor.setData((int) sum);
			sum >>>= 32;

			valueRemaining = sum + highValue;

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
		return returnValue;
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
		InfiniteInteger returnValue = this.copy();
		DequeNode<Integer> returnCursor = returnValue.magnitudeHead;
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
			sum += highSum;

			if(returnCursor.getNext() == null) returnCursor = DequeNode.Factory.createNodeAfter(returnCursor, 0);
			else returnCursor = returnCursor.getNext();
		}
		if(returnCursor.getData().intValue() == 0) returnCursor.remove();  //remove the last node since it is leading 0s
		return returnValue;
	}

	public InfiniteInteger subtract(long value) {
		return subtract(InfiniteInteger.valueOf(value));
	}

	public InfiniteInteger subtract(BigInteger value) {
		return subtract(InfiniteInteger.valueOf(value));
	}

	public InfiniteInteger subtract(InfiniteInteger value) {
		// method stub
		return null;
	}

    public InfiniteInteger multiply(long value) {
		if(value == 0 && this.isInfinite()) return NaN;
    	if(!this.isFinite() || value == 1) return this;
		if(this == ZERO || value == 0) return ZERO;
    	if(value == -1) return this.negate();

		long product, valueRemaining = Math.abs(value);
		InfiniteInteger returnValue = new InfiniteInteger(0);  //can't use ZERO because returnValue will be modified
		InfiniteInteger carry = new InfiniteInteger(0);
		DequeNode<Integer> returnCursor = returnValue.magnitudeHead;
		DequeNode<Integer> carryCursor = carry.magnitudeHead;
		DequeNode<Integer> thisCursor = this.magnitudeHead;

		int lowValue = (int) valueRemaining;
		int highValue = (int) (valueRemaining >>> 32);
		while (thisCursor != null)
		{
			//turns out (true for signed and unsigned) max long > max int * max int. (2^64-1) > ((2^32-1)*(2^32-1))
			product = Integer.toUnsignedLong(thisCursor.getData()) * Integer.toUnsignedLong(lowValue);

			returnCursor.setData((int) product);
			product >>>= 32;
			carryCursor.setData((int) product);

			returnCursor = DequeNode.Factory.createNodeAfter(returnCursor, 0);
			carryCursor = DequeNode.Factory.createNodeAfter(carryCursor, 0);
			thisCursor = thisCursor.getNext();
		}
		if (highValue != 0)
		{
			returnCursor = returnValue.magnitudeHead;
			carryCursor = carry.magnitudeHead.getNext();
			if(carryCursor == null) carryCursor = DequeNode.Factory.createNodeAfter(carry.magnitudeHead, 0);
			thisCursor = this.magnitudeHead;
			while (thisCursor != null)
			{
				//turns out (true for signed and unsigned) max long > max int * max int. (2^64-1) > ((2^32-1)*(2^32-1))
				product = Integer.toUnsignedLong(thisCursor.getData()) * Integer.toUnsignedLong(highValue);

				returnCursor.setData((int) product);
				product >>>= 32;
				carryCursor.setData((int) product);

				if(returnCursor.getNext() == null) returnCursor = DequeNode.Factory.createNodeAfter(returnCursor, 0);
				else returnCursor = returnCursor.getNext();
				if(carryCursor.getNext() == null) carryCursor = DequeNode.Factory.createNodeAfter(carryCursor, 0);
				else carryCursor = carryCursor.getNext();
				thisCursor = thisCursor.getNext();
			}
		}
		if (valueRemaining != 0)
		{
			//the mulitplication's carry causes the return value to have more nodes
			lowValue = (int) valueRemaining;
			highValue = (int) (valueRemaining >>> 32);
			returnCursor.setData(lowValue);
			returnCursor = DequeNode.Factory.createNodeAfter(returnCursor, highValue);
		}
		if(returnCursor.getData().intValue() == 0) returnCursor.remove();  //remove the last node since it is leading 0s
		//do not use else. this can occur either way
		returnValue.isNegative = (isNegative != value < 0);  //!= acts as xor
		//method stub. above is useless. Need to start over
		return null;
    }

    public InfiniteInteger multiply(BigInteger value) {
		return multiply(InfiniteInteger.valueOf(value));
    }

    public InfiniteInteger multiply(InfiniteInteger value) {
		// method stub
		return null;
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
		// method stub
		return null;
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
		if(value.isNegative) return shiftRight(value);

		InfiniteInteger returnValue = this.copy();
		InfiniteInteger valueRemaining = value;
		while (valueRemaining.compareTo(32) != -1)
		{
			returnValue.magnitudeHead = DequeNode.Factory.createNodeBefore(0, returnValue.magnitudeHead);
			valueRemaining = valueRemaining.subtract(32);
		}
		//method stub

		return null;
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
		if(value.isNegative) return shiftLeft(value);

		InfiniteInteger returnValue = this.copy();
		InfiniteInteger valueRemaining = value;
		while (valueRemaining.compareTo(32) != -1)
		{
			returnValue.magnitudeHead = returnValue.magnitudeHead.getNext();
			if(returnValue.magnitudeHead == null) return ZERO;
			returnValue.magnitudeHead.getPrev().remove();
			valueRemaining = valueRemaining.subtract(32);
		}
		//method stub

		return null;
    }

    //TODO: add min/max. maybe static (InfInt, InfInt) only?
    //big int also has bitwise operations. gcd. and weird methods

    public boolean isNaN(){return this == NaN;}
	public boolean isInfinite(){return (this == POSITIVE_INFINITITY || this == NEGATIVE_INFINITITY);}
	public boolean isFinite(){return (!this.isNaN() && !this.isInfinite());}
	public void signalNaN(){if(this == NaN) throw new ArithmeticException("Not a number.");}

	@Override
	public boolean equals(Object obj) {
		if(this == obj) return true;
		if(this == ZERO || this == POSITIVE_INFINITITY || this == NEGATIVE_INFINITITY || this == NaN) return false;
		//these are singletons. if not the same object then it's not equal
		if(!(obj instanceof InfiniteInteger)) return false;  //includes null check and children
		InfiniteInteger other = (InfiniteInteger) obj;
		if(other == ZERO || other == POSITIVE_INFINITITY || other == NEGATIVE_INFINITITY || other == NaN) return false;
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
			stringBuilder.append(Integer.toHexString(cursor.getData().intValue()));
			stringBuilder.append(", ");
		}
		return stringBuilder.toString();
	}

	public void toFile(File writeToHere) {
		// method stub it can always fit
	}
    //I previously implemented writeObject but there doesn't seem to be any way to implement readObject
    //since I don't know how many nodes there are, therefore I deleted writeObject and trust the JVM to serialize

	//javadoc the ones that will not be copied and that being immutable is not all that useful to the outside
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
	//&plusmn;&infin; is as expected
	@Override
	public int compareTo(InfiniteInteger other) {
		byte thisSmaller = -1, thisBigger = 1, sameValue = 0;

		if(this == other) return sameValue;
		if(this == POSITIVE_INFINITITY || other == NEGATIVE_INFINITITY) return thisBigger;
		if(this == NEGATIVE_INFINITITY || other == POSITIVE_INFINITITY) return thisSmaller;

		if (this == NaN)
		{
			if(other == ZERO || other.isNegative) return thisBigger;
			return thisSmaller;  //since other != NaN
		}
		if (other == NaN)
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
		return this.compareTo(InfiniteInteger.valueOf(other));
	}

	public int compareTo(long other) {
		return this.compareTo(InfiniteInteger.valueOf(other));
	}

}
