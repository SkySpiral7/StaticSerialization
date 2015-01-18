package src;

import java.io.File;
import java.math.BigInteger;
import java.util.ListIterator;
import java.util.Objects;

import src.defaultImplementations.DequeNode;
import src.defaultImplementations.DequeNodeIterator;

//aka InfinInt
public class InfiniteInteger extends Number {
	private static final long serialVersionUID = 1L;
	public static final InfiniteInteger ZERO = new InfiniteInteger(0);
	public static final InfiniteInteger NOT_A_NUMBER = new InfiniteInteger(false);
	public static final InfiniteInteger POSITIVE_INFINITITY = new InfiniteInteger(true);
	public static final InfiniteInteger NEGATIVE_INFINITITY = new InfiniteInteger(false);
	protected static final BigInteger bigIntegerMaxLong = BigInteger.valueOf(Long.MAX_VALUE);
	private static InfiniteInteger maxBigInteger;

	/**
	 * Little endian: the first node is the least significant.
	 */
	protected DequeNode<Long> magnitudeHead;
	protected boolean isNegative;

	private InfiniteInteger(boolean isNegative){magnitudeHead = null; this.isNegative = isNegative;}
	protected InfiniteInteger(long value) {
		isNegative = (value < 0);
		magnitudeHead = DequeNode.Factory.createStandAloneNode(Math.abs(value));
	}

	public static InfiniteInteger valueOf(long value) {
		if(value == 0) return ZERO;
		return new InfiniteInteger(value);
	}
	public static InfiniteInteger valueOf(BigInteger value) {
		if(value.compareTo(bigIntegerMaxLong) != 1) return valueOf(value.longValue());
		return ZERO.add(value);
	}

	@Override
	public int intValue() {
		return (int) longValue();
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
		return getBigIntegerValue().longValue();
	}

	public BigInteger getBigIntegerValue() {
		// TODO method stub
		return null;
	}

	public ListIterator<Long> magnitudeIterator() {
		return new ReadOnlyListIterator<Long>(new DequeNodeIterator.IndexAgnosticValueIterator<Long>(magnitudeHead));
	}

	public InfiniteInteger add(long value) {
		//TODO: currently assumes all positive
		if(this == POSITIVE_INFINITITY || this == NOT_A_NUMBER || value == 0) return this;
		long valueRemaining = value;
		InfiniteInteger returnValue = new InfiniteInteger(0);  //can't use ZERO because this will be modified
		DequeNode<Long> returnCursor = returnValue.magnitudeHead;
		DequeNode<Long> thisCursor = this.magnitudeHead;
		while (thisCursor != null)
		{
			long spaceAvailable = (Long.MAX_VALUE - thisCursor.getData());
			if(spaceAvailable >= valueRemaining){returnCursor.setData(thisCursor.getData()+valueRemaining); valueRemaining = 0;}
			returnCursor = DequeNode.Factory.createNodeAfter(returnCursor, 0L);
			thisCursor = thisCursor.getNext();
			//TODO: this means the nodes are all signed...
		}
		returnCursor.remove();  //remove the last one since it has only leading 0s
		return returnValue;
	}

	public InfiniteInteger add(BigInteger value) {
		//TODO: currently assumes all positive
		if(this == POSITIVE_INFINITITY || this == NOT_A_NUMBER) return this;
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
		if(this == POSITIVE_INFINITITY || this == NOT_A_NUMBER || value == ZERO) return this;
		if(value == POSITIVE_INFINITITY || value == NOT_A_NUMBER || this == ZERO) return value;
		// TODO method stub
		return null;
	}

	@Override
	public boolean equals(Object obj) {
		if(this == obj) return true;
		if(this == ZERO || this == POSITIVE_INFINITITY || this == NEGATIVE_INFINITITY || this == NOT_A_NUMBER) return false;
		//these are singletons. if not the same object then it's not equal
		if(!(obj instanceof InfiniteInteger)) return false;  //includes null check and children
		InfiniteInteger other = (InfiniteInteger) obj;
		if(other == ZERO || other == POSITIVE_INFINITITY || other == NEGATIVE_INFINITITY || other == NOT_A_NUMBER) return false;
		DequeNode<Long> thisCursor = this.magnitudeHead;
		DequeNode<Long> otherCursor = other.magnitudeHead;
		while (thisCursor != null && otherCursor != null)
		{
			if(!Objects.equals(thisCursor.getData(), otherCursor.getData())) return false;
			thisCursor = thisCursor.getNext();
			otherCursor = otherCursor.getNext();
		}
		return (thisCursor == otherCursor);  //they must both be null (at end)
	}

	@Override
	public int hashCode() {
		//collisions are, in theory, likely
		DequeNode<Long> cursor = magnitudeHead;
		long hash = cursor.getData();
		cursor = cursor.getNext();
		while (cursor != null)
		{
			hash ^= cursor.getData();
			cursor = cursor.getNext();
		}
		return (int) hash;
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

}
