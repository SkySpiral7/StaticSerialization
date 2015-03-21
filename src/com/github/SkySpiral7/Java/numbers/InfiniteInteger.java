package com.github.SkySpiral7.Java.numbers;

import java.io.File;
import java.math.BigInteger;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import com.github.SkySpiral7.Java.iterators.DequeNodeIterator;
import com.github.SkySpiral7.Java.iterators.ReadOnlyListIterator;
import com.github.SkySpiral7.Java.pojo.DequeNode;
import com.github.SkySpiral7.Java.pojo.IntegerQuotient;
import com.github.SkySpiral7.Java.util.BitWiseUtil;

/**
 * <p>I don't have anything against BigInteger, it's fast and big, but it bugs me that there isn't another class that has no max size.
 * After searching the internet I found a few arbitrary-precision integers but all had a max size.
 * I understand that an array is faster than a linked list and that generating a security key is the only practical
 * way for a primitive long to max out but I wanted the option for infinite precision. And I had fun making it.</p>
 *
 * <p>BigInteger's maximum value (in 1.8) is 2^(2^31-1)-1 which is about 10^(10^8). Using long[] with each element being unsigned
 * would max out at 2^(64 * (2^31-1))-1 which is about 10^(10^10). The largest base 10 number a string can hold is
 * 10^(2^31-1) which is about 10^(10^9) (base 255 has the same estimate). InfiniteInteger on the other hand
 * uses {@code DequeNode<Integer>} internally (which has no limit) each node is unsigned and
 * the nodes are in little endian order. More recently I found
 * <a href="http://mrob.com/pub/perl/hypercalc.html">HyperCalc</a>
 * but that's still not infinite.</p>
 *
 * <p>Someone suggested I should name the class InfinInt but as much as I love puns I would like this class to be
 * taken seriously. There is very little effort put to being efficient therefore expect it to be slow.
 * This class is more of a proof of concept to show that truely infinite precision can be done.
 * This class can logically support more than the hardware can provide. For example:
 * if this object takes 10 TB or memory and you call factorial the logic functions as normal
 * but can't finish the calculation within the next thousand years but of course memory
 * will run out long before then. Since hardware can't be infinite, memory will run out eventually if the number gets too large.</p>
 *
 * <p>This class is immutable and thread-safe. There are constants singletons for Zero, &plusmn;&infin;, and NaN.
 * Zero is a singleton (that object is the only way to reference the number 0) and is defined for convenience such
 * as if(var == ZERO). Positive and negative infinity represent exactly that and may be returned from math operations.
 * NaN is also as expected except that as an object it will have pointer equality NaN == NaN. Operations return NaN
 * instead of throwing whenever possible.</p>
 *
 * <p>But the most useful design is that every operation is overloaded to take either long, BigInteger, or InfiniteInteger
 * which makes var = var.add(1) possible. The methods that exist are based on the ones defined by BigInteger.</p>
 *
 * <p>The constants finite singletons are: Zero, One, Two.</p>
 *
 * @see BigInteger
 */
public class InfiniteInteger extends AbstractInfiniteInteger<InfiniteInteger> {
	private static final long serialVersionUID = 1L;

	/**Common abbreviation for "not a number". This constant is the result of invalid math such as 0/0.
	 * Note that this is a normal object such that <code>(InfiniteInteger.NaN == InfiniteInteger.NaN)</code> is
	 * always true. Therefore it is logically correct unlike the floating point unit's NaN.*/
	public static final InfiniteInteger NaN = new InfiniteInteger(MutableInfiniteInteger.NaN);
	/**+&infin; is a concept rather than a number and can't be the result of math involving finite numbers.
	 * It is defined for completeness and behaves as expected with math resulting in &plusmn;&infin; or NaN.*/
	public static final InfiniteInteger POSITIVE_INFINITITY = new InfiniteInteger(MutableInfiniteInteger.POSITIVE_INFINITITY);
	/**-&infin; is a concept rather than a number and can't be the result of math involving finite numbers.
	 * It is defined for completeness and behaves as expected with math resulting in &plusmn;&infin; or NaN.*/
	public static final InfiniteInteger NEGATIVE_INFINITITY = new InfiniteInteger(MutableInfiniteInteger.NEGATIVE_INFINITITY);

	/**This constant represents 0 and it is the only InfiniteInteger that can be 0 (ie this is a singleton).
	 * Therefore it is safe to use pointer equality for comparison: <code>if(var == InfiniteInteger.ZERO)</code>*/
	public static final InfiniteInteger ZERO = new InfiniteInteger(new MutableInfiniteInteger(0));
	/**This constant represents 1 and it is the only InfiniteInteger that can be 1 (ie this is a singleton).
	 * Therefore it is safe to use pointer equality for comparison: <code>if(var == InfiniteInteger.ONE)</code>*/
	public static final InfiniteInteger ONE = new InfiniteInteger(new MutableInfiniteInteger(1));
	/**This constant represents 2 and it is the only InfiniteInteger that can be 2 (ie this is a singleton).
	 * Therefore it is safe to use pointer equality for comparison: <code>if(var == InfiniteInteger.TWO)</code>*/
	public static final InfiniteInteger TWO = new InfiniteInteger(new MutableInfiniteInteger(2));

	protected MutableInfiniteInteger baseNumber;

	protected InfiniteInteger(MutableInfiniteInteger baseNumber){this.baseNumber = baseNumber;}
	/**
	 * Converts a long value to an InfiniteInteger.
	 *
	 * @param value the desired numeric value
	 * @return a new InfiniteInteger or a defined singleton
	 */
	public static InfiniteInteger valueOf(long value) {
		return valueOf(MutableInfiniteInteger.valueOf(value));
	}

	/**
	 * Converts a BigInteger value to an InfiniteInteger.
	 * Conversion is O(n) and may be slow for large values of the parameter.
	 *
	 * @param value the desired numeric value
	 * @return a new InfiniteInteger or a defined singleton
	 */
	public static InfiniteInteger valueOf(BigInteger value) {
		return valueOf(MutableInfiniteInteger.valueOf(value));
	}

	/**
	 * Converts a MutableInfiniteInteger to an InfiniteInteger.
	 * The value passed in is copied so that this InfiniteInteger won't be affected
	 * by mutating the value.
	 *
	 * @param value the desired numeric value
	 * @return a new InfiniteInteger or a defined singleton
	 */
	public static InfiniteInteger valueOf(MutableInfiniteInteger value) {
		if(value.isNaN()) return NaN;
		if(value == MutableInfiniteInteger.POSITIVE_INFINITITY) return POSITIVE_INFINITITY;
		if(value == MutableInfiniteInteger.NEGATIVE_INFINITITY) return NEGATIVE_INFINITITY;

		if(value.equals(0)) return ZERO;
		if(value.equals(1)) return ONE;
		if(value.equals(2)) return TWO;

		return new InfiniteInteger(value.copy());
	}

	/**
	 * Converts an InfiniteInteger to a MutableInfiniteInteger.
	 * The value returned is a new object so that this InfiniteInteger won't be affected
	 * by mutating the returned object.
	 *
	 * @return a new MutableInfiniteInteger or a defined singleton
	 */
	public MutableInfiniteInteger toMutableInfiniteInteger() {
		return baseNumber.copy();
	}

	/**
	 * Converts an array of UNSIGNED longs into a new InfiniteInteger.
	 * The elements must be in little endian order. This method delegates to littleEndian(Iterator, boolean).
	 * An empty array is considered 0.
	 *
	 * @param valueArray the unsigned elements in little endian order
	 * @param isNegative whether the resulting InfiniteInteger should be negative or not
	 *
	 * @return a new InfiniteInteger representing the indicated number
	 * @see #littleEndian(Iterator, boolean)
	 * @see #bigEndian(long[], boolean)
	 */
	public static InfiniteInteger littleEndian(long[] valueArray, boolean isNegative) {
		return valueOf(MutableInfiniteInteger.littleEndian(valueArray, isNegative));
	}

	/**
	 * Converts an array of UNSIGNED longs into a new InfiniteInteger.
	 * The elements must be in big endian order. This method ultimately delegates to littleEndian(Iterator, boolean).
	 * An empty array is considered 0.
	 *
	 * @param valueArray the unsigned elements in big endian order
	 * @param isNegative whether the resulting InfiniteInteger should be negative or not
	 *
	 * @return a new InfiniteInteger representing the indicated number
	 * @see #bigEndian(Iterator, boolean)
	 * @see #littleEndian(long[], boolean)
	 * @see #littleEndian(Iterator, boolean)
	 */
	public static InfiniteInteger bigEndian(long[] valueArray, boolean isNegative) {
		return valueOf(MutableInfiniteInteger.bigEndian(valueArray, isNegative));
	}

	/**
	 * <p>Converts an iterator of UNSIGNED longs into a new InfiniteInteger.
	 * The elements must be in little endian order.</p>
	 *
	 * <p>The iterator must not return a null element, the meaning of which would be ambiguous.
	 * The iterator can't be infinite since this method aggregates the values (it would also be meaningless).
	 * An empty iterator is considered 0.</p>
	 *
	 * @param valueIterator the unsigned elements in little endian order
	 * @param isNegative whether the resulting InfiniteInteger should be negative or not
	 *
	 * @return a new InfiniteInteger representing the indicated number
	 * @see #littleEndian(long[], boolean)
	 * @see #bigEndian(Iterator, boolean)
	 */
	public static InfiniteInteger littleEndian(Iterator<Long> valueIterator, boolean isNegative) {
		return valueOf(MutableInfiniteInteger.littleEndian(valueIterator, isNegative));
	}

	/**
	 * <p>Converts an iterator of UNSIGNED longs into a new InfiniteInteger.
	 * The elements must be in big endian order. Note that the iterator
	 * must be a list iterator because it must be read backwards.
	 * This method delegates to littleEndian(Iterator, boolean).</p>
	 *
	 * <p>The iterator must not return a null element, the meaning of which would be ambiguous.
	 * The iterator can't be infinite since this method aggregates the values (it would also be meaningless).
	 * An empty iterator is considered 0.</p>
	 *
	 * @param valueIterator the unsigned elements in big endian order
	 * @param isNegative whether the resulting InfiniteInteger should be negative or not
	 *
	 * @return a new InfiniteInteger representing the indicated number
	 * @see #bigEndian(long[], boolean)
	 * @see #littleEndian(Iterator, boolean)
	 */
	public static InfiniteInteger bigEndian(ListIterator<Long> valueIterator, boolean isNegative) {
		return valueOf(MutableInfiniteInteger.bigEndian(valueIterator, isNegative));
	}

	/**
	 * This method returns an infinite stream of all integers.
	 * NaN is not included in the stream and &plusmn;&infin; is unreachable.
	 * The stream is logically truely infinite (will never loop around or overflow)
	 * but hardware will eventually run out of memory.
	 * The stream's order is: 0, 1, -1, 2, -2, 3, -3, 4, -4...
	 *
	 * @return an infinite stream of all integers
	 */
	public static Stream<InfiniteInteger> streamAllIntegers() {
		return Stream.iterate(ZERO, (InfiniteInteger previous) -> {
				if(previous == ZERO) return InfiniteInteger.valueOf(1);
				if(previous.baseNumber.isNegative) return InfiniteInteger.valueOf(previous.baseNumber.copy().abs().add(1));
				return previous.negate();
			});
	}

	/**
	 * <p>This method returns an infinite iterator of all integers.
	 * NaN is not included in the stream and &plusmn;&infin; is unreachable.
	 * The stream is logically truely infinite (will never loop around or overflow)
	 * but hardware will eventually run out of memory.</p>
	 *
	 * <p>The iterator starts after 0 such that calling next() will return 1 and previous() will return 0.
	 * Calling next or previous index will return the intValue(). Set, add, and remove can't be called because
	 * it is read only.</p>
	 *
	 * @return an infinite iterator of all integers
	 * @see #intValue()
	 * @see ReadOnlyListIterator
	 */
	public static ReadOnlyListIterator<InfiniteInteger> iterateAllIntegers() {
        return new ReadOnlyListIterator<>(new ListIterator<InfiniteInteger>() {
            private InfiniteInteger nextElement = InfiniteInteger.valueOf(1);

            @Override public boolean hasNext(){return true;}
            @Override public boolean hasPrevious(){return true;}

            @Override
            public InfiniteInteger next() {
            	InfiniteInteger current = nextElement;
            	nextElement = nextElement.add(1);
                return current;
            }
            @Override
            public InfiniteInteger previous() {
            	nextElement = nextElement.subtract(1);
                return nextElement;
            }

            @Override
			public int nextIndex() {
				return nextElement.intValue();
			}
			@Override
			public int previousIndex() {
				return nextElement.intValue()-1;
			}

			//will be replaced by ReadOnlyListIterator to throw:
			@Override public void remove(){}
			@Override public void set(InfiniteInteger e){}
			@Override public void add(InfiniteInteger e){}
        });
	}

	/**
	 * <p>This method returns an infinite stream all numbers in the Fibonacci Sequence.
	 * The stream starts with 0 which is known as the zeroth element in the sequence.
	 * The stream is logically truely infinite (will never loop around or overflow)
	 * but hardware will eventually run out of memory.</p>
	 *
	 * <p>The Fibonacci Sequence aka golden sequence aka Lame's Sequence is defined by starting with
	 * f(0)=0 and f(1)=1 and f(n)=f(n-2)+f(n-1).
	 * Therefore the sequence starts out with: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34...</p>
	 *
	 * @return an infinite stream of the Fibonacci Sequence
	 */
    public static Stream<InfiniteInteger> streamFibonacciSequence() {
        final Iterator<InfiniteInteger> iterator = new Iterator<InfiniteInteger>() {
            private InfiniteInteger previous = null;
            private InfiniteInteger back2 = null;

            @Override public boolean hasNext(){return true;}

            @Override
            public InfiniteInteger next() {
            	InfiniteInteger next;
                if(previous == null) next = InfiniteInteger.ZERO;
                else if(back2 == null) next = InfiniteInteger.valueOf(1);
                else next = previous.add(back2);
                back2 = previous;
                previous = next;
                return next;
            }
        };
        return StreamSupport.stream(Spliterators.spliteratorUnknownSize(
                iterator,
                Spliterator.ORDERED | Spliterator.IMMUTABLE), false);
    }

    /**
	 * Entire code: <blockquote>{@code return (float) longValue();}</blockquote>
	 * @see #longValue()
	 */
	@Override public float floatValue(){return baseNumber.floatValue();}
	/**
	 * Entire code: <blockquote>{@code return (double) longValue();}</blockquote>
	 * @see #longValue()
	 */
	@Override public double doubleValue(){return baseNumber.doubleValue();}

	/**
	 * This method returns the least significant 31 bits of the number represented by this InfiniteInteger.
	 * The int is then given the same sign as this class. This is different than a narrowing cast because
	 * normally the bits would be unchanged signed or otherwise but this method performs a two's complement.
	 *
	 * @throws ArithmeticException if this is &plusmn;&infin; or NaN
	 * @see #longValue()
	 */
	@Override
	public int intValue() {
		return baseNumber.intValue();
	}

	/**
	 * This method returns the least significant 63 bits of the number represented by this InfiniteInteger.
	 * The long is then given the same sign as this class. This is different than a narrowing cast because
	 * normally the bits would be unchanged signed or otherwise but this method performs a two's complement.
	 *
	 * @throws ArithmeticException if this is &plusmn;&infin; or NaN
	 * @see #longValueExact()
	 * @see #bigIntegerValue()
	 */
	@Override
	public long longValue() {
		return baseNumber.longValue();
	}

	/**
	 * This method returns the longValue only if this InfiniteInteger can fit within a signed long without losing information.
	 *
	 * @throws ArithmeticException if this is &plusmn;&infin; or NaN
	 * @throws ArithmeticException if this is greater than max long: 2^63-1
	 * @see #longValue()
	 * @see #bigIntegerValue()
	 */
	@Override
	public long longValueExact() {
		return baseNumber.longValueExact();
	}

	@Override
	public BigInteger bigIntegerValue() {
		return baseNumber.bigIntegerValue();
	}

	/**
	 * This method returns the a BigInteger representing the same number as this InfiniteInteger.
	 * Or will throw if this InfiniteInteger is greater than BigInteger will allow.
	 *
	 * @throws ArithmeticException if this is &plusmn;&infin; or NaN
	 * @throws ArithmeticException if this is greater than the max of BigInteger: 2^(2^31-1)-1
	 * @see #bigIntegerValue()
	 * @see #longValue()
	 */
	@Override
	public BigInteger bigIntegerValueExact() {
		return baseNumber.bigIntegerValueExact();
	}

	/**
	 * This method returns a read only list iterator of unknown size that iterates over the data of each of the nodes
	 * of this InfiniteInteger. Each node is unsigned and they are in little endian order.
	 * Calling nextIndex or previousIndex will return -1 and calling add, set, or remove will throw.
	 * Note that there might be more than Long.Max elements (or even max BigInteger!).
	 *
	 * @throws UnsupportedOperationException if this is &plusmn;&infin; or NaN
	 * @see #magnitudeStream()
	 * @see ReadOnlyListIterator
	 * @see DequeNodeIterator.IndexAgnosticValueIterator
	 */
	@Override
	public ReadOnlyListIterator<Integer> magnitudeIterator() {
		return baseNumber.magnitudeIterator();
	}

	/**
	 * This method returns a stream for the data of each of the nodes of this InfiniteInteger.
	 * Each node is unsigned and they are in little endian order.
	 * Note that there might be more than Long.Max elements (or even max BigInteger!).
	 * This method represents that there can be any number of elements better than magnitudeIterator.
	 * Streams are also naturally read only with unknown size.
	 *
	 * @throws UnsupportedOperationException if this is &plusmn;&infin; or NaN
	 * @see #magnitudeIterator()
	 */
	@Override
	public Stream<Integer> magnitudeStream() {
		return baseNumber.magnitudeStream();
	}

	/**
	 * Helper method to get the last (most significant) node of this InfiniteInteger.
	 * @throws NullPointerException if magnitudeHead is null
	 */
	@Override
	protected DequeNode<Integer> getMagnitudeTail() {
		return baseNumber.getMagnitudeTail();
	}

    /**
     * Returns an InfiniteInteger whose value is {@code (this + value)}.
     * Note that the formula used is designed for a long and is slightly more efficient
     * than calling add(InfiniteInteger.valueOf(value)) would be.
     *
     * @param  value the operand to be added to this InfiniteInteger.
     * @return the result including &plusmn;&infin; and NaN
     * @see #add(InfiniteInteger)
     */
	@Override
	public InfiniteInteger add(long value) {
		return valueOf(baseNumber.copy().add(value));
	}

	/**
	 * Entire code: <blockquote>{@code return this.add(InfiniteInteger.valueOf(value));}</blockquote>
	 * @see #add(InfiniteInteger)
	 * @see #valueOf(BigInteger)
	 */
	@Override
	public InfiniteInteger add(BigInteger value) {
		return valueOf(baseNumber.copy().add(value));
	}

    /**
     * Returns an InfiniteInteger whose value is {@code (this + value)}.
     *
     * @param  value the operand to be added to this InfiniteInteger.
     * @return the result including &plusmn;&infin; and NaN
     * @see #add(long)
     */
	@Override
	public InfiniteInteger add(InfiniteInteger value) {
		return valueOf(baseNumber.copy().add(value.baseNumber));
	}

    /**
     * Returns an InfiniteInteger whose value is {@code (this - value)}.
     * Note that the formula used is designed for a long and is slightly more efficient
     * than calling subtract(InfiniteInteger.valueOf(value)) would be.
     *
     * @param  value the operand to be subtracted from this InfiniteInteger.
     * @return the result including &plusmn;&infin; and NaN
     * @see #subtract(InfiniteInteger)
     */
	@Override
	public InfiniteInteger subtract(long value) {
		return valueOf(baseNumber.copy().subtract(value));
	}

	/**
	 * Entire code: <blockquote>{@code return this.subtract(InfiniteInteger.valueOf(value));}</blockquote>
	 * @see #subtract(InfiniteInteger)
	 * @see #valueOf(BigInteger)
	 */
	@Override
	public InfiniteInteger subtract(BigInteger value) {
		return valueOf(baseNumber.copy().subtract(value));
	}

    /**
     * Returns an InfiniteInteger whose value is {@code (this - value)}.
     * Note &infin; - &infin; results in NaN.
     *
     * @param  value the operand to be subtracted from this InfiniteInteger.
     * @return the result including &plusmn;&infin; and NaN
     * @see #subtract(long)
     */
	@Override
	public InfiniteInteger subtract(InfiniteInteger value) {
		return valueOf(baseNumber.copy().subtract(value.baseNumber));
	}

    /**
     * Returns an InfiniteInteger whose value is {@code (this * value)}.
     * Note that the formula used is designed for a long and is slightly more efficient
     * than calling multiply(InfiniteInteger.valueOf(value)) would be.
     * Note &plusmn;&infin; * 0 results in NaN.
     *
     * @param  value the operand to be multiplied to this InfiniteInteger.
     * @return the result including &plusmn;&infin; and NaN
     * @see #multiply(InfiniteInteger)
     */
	@Override
    public InfiniteInteger multiply(long value) {
		return valueOf(baseNumber.copy().multiply(value));
	}

	/**
	 * Entire code: <blockquote>{@code return this.multiply(InfiniteInteger.valueOf(value));}</blockquote>
	 * @see #multiply(InfiniteInteger)
	 * @see #valueOf(BigInteger)
	 */
	@Override
    public InfiniteInteger multiply(BigInteger value) {
		return valueOf(baseNumber.copy().multiply(value));
	}

    /**
     * Returns an InfiniteInteger whose value is {@code (this * value)}.
     * Note &plusmn;&infin; * 0 results in NaN.
     *
     * @param  value the operand to be multiplied to this InfiniteInteger.
     * @return the result including &plusmn;&infin; and NaN
     * @see #multiply(long)
     */
	@Override
    public InfiniteInteger multiply(InfiniteInteger value) {
		return valueOf(baseNumber.copy().multiply(value.baseNumber));
	}

    /**
	 * This method delegates because the formula used is exactly the same.
	 * Entire code: <blockquote>{@code return this.multiplyByPowerOf2(InfiniteInteger.valueOf(exponent));}</blockquote>
	 *
	 * @see #multiplyByPowerOf2(InfiniteInteger)
	 * @see #valueOf(long)
	 */
	@Override
	public InfiniteInteger multiplyByPowerOf2(long exponent) {
		return valueOf(baseNumber.copy().multiplyByPowerOf2(exponent));
	}

	/**
	 * Entire code: <blockquote>{@code return this.multiplyByPowerOf2(InfiniteInteger.valueOf(exponent));}</blockquote>
	 * @see #multiplyByPowerOf2(InfiniteInteger)
	 * @see #valueOf(BigInteger)
	 */
	@Override
	public InfiniteInteger multiplyByPowerOf2(BigInteger exponent) {
		return valueOf(baseNumber.copy().multiplyByPowerOf2(exponent));
	}

	/**
	 * <p>Returns an InfiniteInteger whose value is {@code (this << exponent)}.
	 * If the exponent is negative then a right shift is performed instead.
	 * Computes <tt>this * 2<sup>exponent</sup></tt>.
	 * Note that the nodes are unsigned and this operation ignores sign.
	 * Therefore this operation won't change the sign.</p>
	 *
	 * <p>Examples:<br /><code>
	 * InfiniteInteger.valueOf(-10).multiplyByPowerOf2(1) is -20<br />
	 * InfiniteInteger.valueOf(100).multiplyByPowerOf2(2) is 400</code></p>
	 *
	 * <p>This method is not named shiftLeft because the direction left only makes sense for big endian numbers.</p>
	 *
	 * @param  exponent is also the shift distance in bits
	 * @return the result including &plusmn;&infin; and NaN
	 * @see #divideByPowerOf2DropRemainder(InfiniteInteger)
	 */
	@Override
	public InfiniteInteger multiplyByPowerOf2(InfiniteInteger exponent) {
		return valueOf(baseNumber.copy().multiplyByPowerOf2(exponent.baseNumber));
	}

	@Override
	public IntegerQuotient<InfiniteInteger> divide(long value) {
		return divide(InfiniteInteger.valueOf(value));
    }

	/**
	 * Entire code: <blockquote>{@code return this.divide(InfiniteInteger.valueOf(value));}</blockquote>
	 * @see #divide(InfiniteInteger)
	 * @see #valueOf(BigInteger)
	 */
	@Override
    public IntegerQuotient<InfiniteInteger> divide(BigInteger value){return this.divide(InfiniteInteger.valueOf(value));}

	@Override
    public IntegerQuotient<InfiniteInteger> divide(InfiniteInteger value) {
		IntegerQuotient<MutableInfiniteInteger> mutableAnswer = baseNumber.copy().divide(value.baseNumber);
		return new IntegerQuotient<InfiniteInteger>(valueOf(mutableAnswer.getWholeResult()), valueOf(mutableAnswer.getRemainder()));
    }

    //aka divideReturnWhole
	@Override
    public InfiniteInteger divideDropRemainder(long value) {
		return valueOf(baseNumber.copy().divideDropRemainder(value));
    }

	/**
	 * Entire code: <blockquote>{@code return this.divideDropRemainder(InfiniteInteger.valueOf(value));}</blockquote>
	 * @see #divideDropRemainder(InfiniteInteger)
	 * @see #valueOf(BigInteger)
	 */
	@Override
    public InfiniteInteger divideDropRemainder(BigInteger value) {
		return valueOf(baseNumber.copy().divideDropRemainder(value));
	}

	@Override
    public InfiniteInteger divideDropRemainder(InfiniteInteger value) {
		return valueOf(baseNumber.copy().divideDropRemainder(value.baseNumber));
    }

    /**
	 * This method delegates because the formula used is exactly the same.
	 * Entire code: <blockquote>{@code return this.divideByPowerOf2DropRemainder(InfiniteInteger.valueOf(exponent));}</blockquote>
	 *
	 * @see #divideByPowerOf2DropRemainder(InfiniteInteger)
	 * @see #valueOf(long)
	 */
	@Override
	public InfiniteInteger divideByPowerOf2DropRemainder(long exponent) {
		return valueOf(baseNumber.copy().divideByPowerOf2DropRemainder(exponent));
	}

	/**
	 * Entire code: <blockquote>{@code return this.divideByPowerOf2DropRemainder(InfiniteInteger.valueOf(exponent));}</blockquote>
	 * @see #divideByPowerOf2DropRemainder(InfiniteInteger)
	 * @see #valueOf(BigInteger)
	 */
	@Override
	public InfiniteInteger divideByPowerOf2DropRemainder(BigInteger exponent) {
		return valueOf(baseNumber.copy().divideByPowerOf2DropRemainder(exponent));
	}

	/**
	 * <p>Returns an InfiniteInteger whose value is {@code (this >>> exponent)}.
	 * If the exponent is negative then a left shift is performed instead.
	 * Computes <tt>truncate(this / 2<sup>exponent</sup>)</tt>.
	 * Note that the nodes are unsigned and this operation ignores sign.
	 * Also note that truncation occurs which means the low numbers are thrown away not rounded.
	 * Therefore this operation always 0 fills and won't change the sign (unless the result is ZERO).</p>
	 * <p>Examples:<br /><code>
	 * InfiniteInteger.valueOf(-10).divideByPowerOf2DropRemainder(1) is -5<br />
	 * InfiniteInteger.valueOf(100).divideByPowerOf2DropRemainder(2) is 25<br />
	 * InfiniteInteger.valueOf(3).divideByPowerOf2DropRemainder(1) is 1</code></p>
	 *
	 * <p>This method is not named shiftRight because the direction right only makes sense for big endian numbers.</p>
	 *
	 * @param  exponent is also the shift distance in bits
	 * @return the result including &plusmn;&infin; and NaN
	 * @see #multiplyByPowerOf2(InfiniteInteger)
	 */
	@Override
	public InfiniteInteger divideByPowerOf2DropRemainder(InfiniteInteger exponent) {
		return valueOf(baseNumber.copy().divideByPowerOf2DropRemainder(exponent.baseNumber));
	}

	//aka remainder, divideDropWhole, divideReturnRemainder
	@Override
    public InfiniteInteger divideReturnRemainder(long value) {
		return valueOf(baseNumber.copy().divideReturnRemainder(value));
    }

	/**
	 * Entire code: <blockquote>{@code return this.mod(InfiniteInteger.valueOf(value));}</blockquote>
	 * @see #divideReturnRemainder(InfiniteInteger)
	 * @see #valueOf(BigInteger)
	 */
	@Override
    public InfiniteInteger divideReturnRemainder(BigInteger value) {
		return valueOf(baseNumber.copy().divideReturnRemainder(value));
	}

	@Override
    public InfiniteInteger divideReturnRemainder(InfiniteInteger value) {
		return valueOf(baseNumber.copy().divideReturnRemainder(value.baseNumber));
    }

	@Override
    public InfiniteInteger power(long exponent) {
		return valueOf(baseNumber.copy().power(exponent));
    }

	/**
	 * Entire code: <blockquote>{@code return this.pow(InfiniteInteger.valueOf(exponent));}</blockquote>
	 * @see #power(InfiniteInteger)
	 * @see #valueOf(BigInteger)
	 */
	@Override
    public InfiniteInteger power(BigInteger exponent) {
		return valueOf(baseNumber.copy().power(exponent));
	}

	//BigInt is wrong: 0^0 is NaN but it returns 1. And 1^(-2) is 1 but it throws
	//TODO: make this table into a class that can't be modified
	/**
	 * Used by powerSpecialLookUp. Private to prevent modification.
	 * @see #powerSpecialLookUp(InfiniteInteger, InfiniteInteger)
	 */
	private final static InfiniteInteger[][] powerSpecialCaseTable = {
		//the elements are in order: 0, 1, Infinity, -Infinity, -X (other), X (other)
		{NaN, ZERO, NaN, NaN, NaN, ZERO}, //0
		{ONE, ONE, NaN, NaN, ONE, ONE},  //1
		{NaN, POSITIVE_INFINITITY, POSITIVE_INFINITITY, ZERO, ZERO, POSITIVE_INFINITITY},  //Infinity
		{NaN, NEGATIVE_INFINITITY, NaN, NaN, ZERO, null},  //-Infinity
		{ONE, null, NaN, ZERO, null, null},  //-X (other)
		{ONE, null, POSITIVE_INFINITITY, ZERO, null, null}  //X (other)
	};

/**
 *<table border="1">
 * <caption><b>Special cases for Base<sup>Exponent</sup></b></caption>
 * <tr><th></th><th colspan="6">Exponent</th></tr>
 * <tr valign="top">
 *                    <th>Base</th>     <th width="30px">0</th>
 *                                                   <th width="30px">1</th>
 *                                                                     <th width="30px">&infin;</th>
 *                                                                                      <th width="30px">-&infin;</th>
 *                                                                                                   <th width="30px">-X</th>
 *                                                                                                                <th width="30px">X</th></tr>
 *
 * <tr align="center"><td>0</td>        <td>NaN</td> <td>0</td>        <td>NaN</td>     <td>NaN</td> <td>NaN</td> <td>0</td></tr>
 * <tr align="center"><td>1</td>        <td>1</td>   <td>1</td>        <td>NaN</td>     <td>NaN</td> <td>1</td>   <td>1</td></tr>
 * <tr align="center"><td>&infin;</td>  <td>NaN</td> <td>&infin;</td>  <td>&infin;</td> <td>0</td>   <td>0</td>   <td>&infin;</td></tr>
 * <tr align="center"><td>-&infin;</td> <td>NaN</td> <td>-&infin;</td> <td>NaN</td>     <td>NaN</td> <td>0</td>   <td>&plusmn;&infin;</td></tr>
 * <tr align="center"><td>-X</td>       <td>1</td>   <td>-X</td>       <td>NaN</td>     <td>0</td>   <td>1/?</td> <td>?</td></tr>
 * <tr align="center"><td>X</td>        <td>1</td>   <td>X</td>        <td>&infin;</td> <td>0</td>   <td>1/?</td> <td>?</td></tr>
 *</table>
 *
 * <p>In the table above X is an integer greater than one. 1/? means the result is a
 * fraction instead of an integer. And ? means that the answer is an integer but this method doesn't know the exact value.
 * In the cases of 1/? and ? null is returned. In all other cases the answer is returned.</p>
 *
 * @param base
 * @param exponent
 * @return the answer or null
 */
	protected static InfiniteInteger powerSpecialLookUp(InfiniteInteger base, InfiniteInteger exponent) {
		if(base.isNaN() || exponent.isNaN()) return NaN;
		if(exponent.equals(1)) return base;  //always true
		//TODO: test all these special cases of pow

		byte baseIndex = powerSpecialIndex(base);
		byte exponentIndex = powerSpecialIndex(exponent);  //is never 1 due to above if check
		InfiniteInteger tableValue = powerSpecialCaseTable[baseIndex][exponentIndex];

		if(tableValue != null) return tableValue;

		if (base == NEGATIVE_INFINITITY)
		{
			//exponent.isFinite by this point (exponentIndex == 5 for X)
			if(BitWiseUtil.isEven(exponent.intValue())) return POSITIVE_INFINITITY;
			return NEGATIVE_INFINITITY;
		}

		//baseIndex == 4 or 5 and exponentIndex == 4 or 5 for -X or X. in all 4 cases return null
		return null;
	}

	/**
	 * Used by powerSpecialLookUp to find the table index to use for a given value.
	 *
	 * @param value
	 * @return the table index which matches the powerSpecialCaseTable
	 * @see #powerSpecialLookUp(InfiniteInteger, InfiniteInteger)
	 * @see #powerSpecialCaseTable
	 */
	protected static byte powerSpecialIndex(InfiniteInteger value) {
		if(value == ZERO) return 0;
		if(value == ONE) return 1;
		if(value == POSITIVE_INFINITITY) return 2;
		if(value == NEGATIVE_INFINITITY) return 3;
		if(value.baseNumber.isNegative) return 4;
		return 5;
	}

	/**
     * Returns an InfiniteInteger whose value is this<sup>exponent</sup>.
     * There are many special cases, for a full table see {@link InfiniteInteger#powerSpecialLookUp(InfiniteInteger, InfiniteInteger) this table}
     * except the pow method will return the result instead of null.
     *
     * @param  exponent to which this InfiniteInteger is to be raised.
     * @return the result including &plusmn;&infin; and NaN
     * @throws ArithmeticException if the result would be a fraction (only possible if exponent is negative)
     */
	@Override
    public InfiniteInteger power(InfiniteInteger exponent) {
		return valueOf(baseNumber.copy().power(exponent.baseNumber));
    }

    /**
     * Returns an InfiniteInteger whose value is this<sup>this</sup>.
     * This method exists mostly as a testimony that this class really can hold any integer.
     * The result will be much larger than a factorial so it will be a slow execution.
     * For example if this InfiniteInteger is 3 then 3<sup>3</sup> is 27.
     *
     * @return the result including +&infin; and NaN
     * @see #power(InfiniteInteger)
     */
	@Override
    public InfiniteInteger selfPower() {
		return valueOf(baseNumber.copy().selfPower());
	}

    /**
     * Returns an InfiniteInteger whose value is this!.
     * This method exists mostly as a testimony that this class really can hold any integer.
     * Factorial is defined as a number multiplied by all positive integers less than it.
     * So 4! = 4*3*2*1. The special cases of 1! and 0! are 1 but factorial is not defined for
     * negative numbers. If this InfiniteInteger is negative then NaN is returned.
     *
     * @return the result including +&infin; and NaN
     * @see #power(InfiniteInteger)
     */
	@Override
    public InfiniteInteger factorial() {
		return valueOf(baseNumber.copy().factorial());
	}

    /**
     * Returns the absolute value of this InfiniteInteger.
     *
     * @return itself or the positive version of this
     * @see Math#abs(double)
     */
	@Override
    public InfiniteInteger abs() {
		return valueOf(baseNumber.copy().abs());
	}

    /**
     * Returns an InfiniteInteger whose value is {@code (0-this)}.
     *
     * @return {@code -this}
     */
	@Override
    public InfiniteInteger negate() {
		return valueOf(baseNumber.copy().negate());
	}

    /**
     * @return -1, 0 or 1 as the value of this number is negative, zero or
     *         positive respectively. NaN returns 0.
     */
	@Override
    public byte signum() {
		return baseNumber.signum();
	}

    /**
     * Compares this == NaN.
     * @return true if this InfiniteInteger is the constant for NaN.
     * @see #NaN
     */
	@Override
    public boolean isNaN(){return this == NaN;}
    /**
     * Compares this InfiniteInteger to both positive and negative infinity.
     * @return true if this InfiniteInteger is either of the infinity constants.
     * @see #POSITIVE_INFINITITY
     * @see #NEGATIVE_INFINITITY
     */
	@Override
	public boolean isInfinite(){return (this == POSITIVE_INFINITITY || this == NEGATIVE_INFINITITY);}
    /**
     * Compares this InfiniteInteger to &plusmn;&infin; and NaN (returns false of this is any of them).
     * @return true if this InfiniteInteger is not a special value (ie if this is a finite number).
     * @see #NaN
     * @see #POSITIVE_INFINITITY
     * @see #NEGATIVE_INFINITITY
     */
	@Override
	public boolean isFinite(){return (!this.isNaN() && !this.isInfinite());}
	/**
	 * @throws ArithmeticException if this == NaN
	 */
	@Override
	public void signalNaN(){if(isNaN()) throw new ArithmeticException("Not a number.");}

	/**
	 * Compares this InfiniteInteger with the specified object for numeric equality.
	 * Note that this equality is not always symmetric as other.equals(this) != this.equals(other).
	 *
	 * @param other the value to be compared to this
	 * @return true if this InfiniteInteger has the same numeric value as other. false if other is not a number
	 * @see #equals(InfiniteInteger)
	 */
	@Override
	public boolean equals(Object other) {
		if(other == null) return false;
		if(other instanceof InfiniteInteger) return this.equals((InfiniteInteger) other);  //checks this == other
		if(other instanceof BigInteger) return this.equals(InfiniteInteger.valueOf((BigInteger) other));
		if(!this.isFinite()) return false;
		if(other instanceof Number) return this.equals(((Number) other).longValue());
		return false;
	}

	/**
	 * Compares this InfiniteInteger with the specified object for numeric equality.
	 *
	 * @param other the value to be compared to this
	 * @return true if this InfiniteInteger has the same numeric value as other
	 * @see #compareTo(InfiniteInteger)
	 */
	@Override
	public boolean equals(InfiniteInteger other) {
		return baseNumber.equals(other.baseNumber);
	}

	/**
	 * Compares this InfiniteInteger with the specified value for numeric equality.
	 * Note that this equality is symmetric with Long.valueOf(value).equals(this.longValueExact())
	 * only if this <= Long.MAX_VALUE.
	 *
	 * @param value the value to be compared to this
	 * @return true if this InfiniteInteger has the same numeric value as the value parameter
	 * @see #longValueExact()
	 * @see #compareTo(long)
	 */
	@Override
	public boolean equals(long value) {
		return baseNumber.equals(value);
	}

	/**
	 * Compares this InfiniteInteger with the specified other for numeric equality.
	 * The natural order is as expected with &plusmn;&infin; being at either end.
	 * However for the sake of sorting 0 < NaN < 1.
	 *
	 * @param other the value to be compared to this
	 * @return -1, 0 or 1 if this InfiniteInteger is numerically less than, equal
	 *         to, or greater than other.
	 */
	@Override
	public int compareTo(InfiniteInteger other) {
		return baseNumber.compareTo(other.baseNumber);
	}

	/**
	 * Compares this InfiniteInteger with the specified other for numeric equality.
	 * Even though sorting is not possible this method returns as expected.
	 * Entire code: <blockquote>{@code return this.compareTo(InfiniteInteger.valueOf(other));}</blockquote>
	 *
	 * @param other the value to be compared to this
	 * @see #compareTo(InfiniteInteger)
	 * @see Comparable#compareTo(Object)
	 */
	@Override
	public int compareTo(BigInteger other){return this.compareTo(InfiniteInteger.valueOf(other));}

	/**
	 * Compares this InfiniteInteger with the specified other for numeric equality.
	 * Even though sorting is not possible this method returns as expected.
	 * Entire code: <blockquote>{@code return this.compareTo(InfiniteInteger.valueOf(other));}</blockquote>
	 *
	 * @param other the value to be compared to this
	 * @see #compareTo(InfiniteInteger)
	 * @see Comparable#compareTo(Object)
	 */
	@Override
	public int compareTo(long other){return this.compareTo(InfiniteInteger.valueOf(other));}

	/**
     * Returns the hash code for this InfiniteInteger.
     * Collisions are, in theory, likely when comparing all possible integers
     * with all possible values that can fit into int.
     *
     * @return hash code for this InfiniteInteger.
     */
	@Override
	public int hashCode() {
		return baseNumber.hashCode();
	}

	@Override
	public String toString() {
		return baseNumber.toString();
	}

	@Override
	public void toFile(File writeToHere) {
		// method stub it can always fit
	}
    //I could implement writeObject and readObject but the JVM default serialization works fine.
	//readObject is possible by putting a long for count of following nodes that exist and repeat
	//for example if there were Long.max+1 nodes then serialize: Long.max, all but last node, 1, last node

}
