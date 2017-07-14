package com.github.SkySpiral7.Java.numbers;

import java.io.File;
import java.math.BigInteger;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.Objects;
import java.util.Random;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import com.github.SkySpiral7.Java.Copyable;
import com.github.SkySpiral7.Java.iterators.DequeNodeIterator;
import com.github.SkySpiral7.Java.iterators.DescendingListIterator;
import com.github.SkySpiral7.Java.iterators.ReadOnlyListIterator;
import com.github.SkySpiral7.Java.pojo.DequeNode;
import com.github.SkySpiral7.Java.pojo.IntegerQuotient;
import com.github.SkySpiral7.Java.util.BitWiseUtil;
import com.github.SkySpiral7.Java.util.ComparableSugar;

import static com.github.SkySpiral7.Java.pojo.Comparison.EQUAL_TO;
import static com.github.SkySpiral7.Java.pojo.Comparison.GREATER_THAN;
import static com.github.SkySpiral7.Java.pojo.Comparison.GREATER_THAN_OR_EQUAL_TO;
import static com.github.SkySpiral7.Java.pojo.Comparison.LESS_THAN;
import static com.github.SkySpiral7.Java.pojo.Comparison.LESS_THAN_OR_EQUAL_TO;
import static com.github.SkySpiral7.Java.util.ComparableSugar.THIS_EQUAL;
import static com.github.SkySpiral7.Java.util.ComparableSugar.THIS_GREATER;
import static com.github.SkySpiral7.Java.util.ComparableSugar.THIS_LESSER;
import static com.github.SkySpiral7.Java.util.ComparableSugar.is;
import static com.github.SkySpiral7.Java.util.ComparableSugar.isComparisonResult;

/**
 * A Mutable version of InfiniteInteger for the sake of speed.
 *
 * @see InfiniteInteger
 */
//TODO: move all this class doc to the Abstract
public final class MutableInfiniteInteger extends AbstractInfiniteInteger<MutableInfiniteInteger>
      implements Copyable<MutableInfiniteInteger>
{
   private static final long serialVersionUID = 1L;

   /**
    * Common abbreviation for "not a number". This constant is the result of invalid math such as 0/0.
    * Note that this is a normal object such that <code>(InfiniteInteger.NaN == InfiniteInteger.NaN)</code> is
    * always true. Therefore it is logically correct unlike the floating point unit's NaN.
    *
    * This value is immutable.
    */
   //future results for NaN: x/0 (including 0), |inifn|/|inifn|, 0^0, 1^|inifn|, |inifn|^0
   public static final MutableInfiniteInteger NaN = new MutableInfiniteInteger(false);
   /**
    * +&infin; is a concept rather than a number and can't be the result of math involving finite numbers.
    * It is defined for completeness and behaves as expected with math resulting in &plusmn;&infin; or NaN.
    *
    * This value is immutable.
    */
   public static final MutableInfiniteInteger POSITIVE_INFINITITY = new MutableInfiniteInteger(true);
   /**
    * -&infin; is a concept rather than a number and can't be the result of math involving finite numbers.
    * It is defined for completeness and behaves as expected with math resulting in &plusmn;&infin; or NaN.
    *
    * This value is immutable.
    */
   public static final MutableInfiniteInteger NEGATIVE_INFINITITY = new MutableInfiniteInteger(false);

   /**
    * Little endian: the first node is the least significant.
    */
   protected DequeNode<Integer> magnitudeHead;
   protected boolean isNegative;

   /**
    * This constructor is used to make special constants.
    * This makes the head null which is something that is otherwise not possible.
    *
    * @param isNegative used for positive and negative infinity. Meaningless to NaN.
    */
   protected MutableInfiniteInteger(boolean isNegative)
   {
      magnitudeHead = null;
      this.isNegative = isNegative;
   }

   /**
    * This constructor is used to create an InfiniteInteger of a small size.
    *
    * @param baseValue the desired numeric value
    *
    * @see #valueOf(long)
    */
   public MutableInfiniteInteger(long baseValue)
   {
      if (baseValue == Long.MIN_VALUE)  //Math.abs isn't possible for this
      {
         isNegative = true;
         magnitudeHead = DequeNode.Factory.createStandAloneNode(0);
         DequeNode.Factory.createNodeAfter(magnitudeHead, Integer.MIN_VALUE);
      }
      else
      {
         isNegative = (baseValue < 0);
         baseValue = Math.abs(baseValue);
         magnitudeHead = DequeNode.Factory.createStandAloneNode((int) baseValue);
         baseValue >>>= 32;
         if (baseValue > 0) DequeNode.Factory.createNodeAfter(magnitudeHead, ((int) baseValue));
      }
   }

   /**
    * Converts a long value to an InfiniteInteger. This is simply an alias for the constructor.
    *
    * @param value the desired numeric value
    *
    * @return a new InfiniteInteger
    *
    * @see #MutableInfiniteInteger(long)
    */
   public static MutableInfiniteInteger valueOf(long value)
   {
      return new MutableInfiniteInteger(value);
   }
   //TODO: make a valueOf(double) for Infinity and NaN. otherwise cast to long

   //new MutableInfiniteInteger(BigInteger) doesn't exist because valueOf(BigInteger) uses fast paths (and I don't want a slow constructor)

   /**
    * Converts a BigInteger value to an InfiniteInteger.
    * Conversion is O(n) and may be slow for large values of the parameter.
    *
    * @param value the desired numeric value
    *
    * @return a new InfiniteInteger
    */
   public static MutableInfiniteInteger valueOf(BigInteger value)
   {
      if (value.equals(BigInteger.ZERO)) return new MutableInfiniteInteger(0);
      final boolean willBeNegative = (value.signum() == -1);  //don't need to use < 0 because of signum's promise
      BigInteger valueRemaining = value.abs();

      final BigInteger bigIntegerMaxLong = BigInteger.valueOf(Long.MAX_VALUE);
      if (is(valueRemaining, LESS_THAN_OR_EQUAL_TO, bigIntegerMaxLong)) return MutableInfiniteInteger.valueOf(value.longValue());
      //if abs fits in a signed long then delegate (original value)

      MutableInfiniteInteger result = new MutableInfiniteInteger(0);

      while (is(valueRemaining, GREATER_THAN, bigIntegerMaxLong))
      {
         //TODO: performance: shift up me and add value's most significant int then shift it down
         //call byte array to get the most significant
         result = result.add(Long.MAX_VALUE);
         valueRemaining = valueRemaining.subtract(bigIntegerMaxLong);
      }
      result = result.add(valueRemaining.longValue());
      result.isNegative = willBeNegative;
      return result;
   }

   /**
    * Copy constructor.
    * Protected in order to maintain singletons.
    *
    * @see #copy()
    * @see #set(MutableInfiniteInteger)
    */
   protected MutableInfiniteInteger(MutableInfiniteInteger value)
   {
      this.set(value);
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
    *
    * @see #littleEndian(Iterator, boolean)
    * @see #bigEndian(long[], boolean)
    */
   public static MutableInfiniteInteger littleEndian(long[] valueArray, boolean isNegative)
   {
      Long[] wrappedValues = new Long[valueArray.length];
      for (int i = 0; i < valueArray.length; i++){wrappedValues[i] = Long.valueOf(valueArray[i]);}
      return littleEndian(Arrays.asList(wrappedValues).listIterator(), isNegative);
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
    *
    * @see #bigEndian(ListIterator, boolean)
    * @see #littleEndian(long[], boolean)
    * @see #littleEndian(Iterator, boolean)
    */
   public static MutableInfiniteInteger bigEndian(long[] valueArray, boolean isNegative)
   {
      Long[] wrappedValues = new Long[valueArray.length];
      for (int i = 0; i < valueArray.length; i++){wrappedValues[i] = Long.valueOf(valueArray[i]);}
      return bigEndian(Arrays.asList(wrappedValues).listIterator(), isNegative);
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
    * @param isNegative    whether the resulting InfiniteInteger should be negative or not
    *
    * @return a new InfiniteInteger representing the indicated number
    *
    * @see #littleEndian(long[], boolean)
    * @see #bigEndian(ListIterator, boolean)
    */
   public static MutableInfiniteInteger littleEndian(Iterator<Long> valueIterator, boolean isNegative)
   {
      if (!valueIterator.hasNext()) return new MutableInfiniteInteger(0);
      MutableInfiniteInteger result = MutableInfiniteInteger.valueOf(valueIterator.next());
      result.isNegative = isNegative;
      DequeNode<Integer> cursor = result.magnitudeHead;
      if (cursor.getNext() == null) cursor = DequeNode.Factory.createNodeAfter(cursor, 0);
      while (valueIterator.hasNext())
      {
         long value = valueIterator.next();
         cursor = DequeNode.Factory.createNodeAfter(cursor, (int) value);
         value >>>= 32;
         cursor = DequeNode.Factory.createNodeAfter(cursor, (int) value);
      }
      while (cursor.getData().intValue() == 0)  //cursor is always at the last node. so remove leading 0s
      {
         cursor = cursor.getPrev();
         if (cursor == null) return new MutableInfiniteInteger(0);  //if the last and only node was 0
         cursor.getNext().remove();
      }
      return result;
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
    * @param isNegative    whether the resulting InfiniteInteger should be negative or not
    *
    * @return a new InfiniteInteger representing the indicated number
    *
    * @see #bigEndian(long[], boolean)
    * @see #littleEndian(Iterator, boolean)
    */
   public static MutableInfiniteInteger bigEndian(ListIterator<Long> valueIterator, boolean isNegative)
   {
      return littleEndian(DescendingListIterator.iterateBackwardsFromEnd(valueIterator), isNegative);
   }

   /**
    * Constructs a randomly generated InfiniteInteger, uniformly distributed over
    * the range 0 to 2^(32 * {@code nodeCount}), inclusive.
    * The uniformity of the distribution assumes the Random class is a fair source of random
    * bits. Note that the result may be positive or negative.
    *
    * @return NaN if {@code nodeCount} < 1 otherwise a new random number is returned.
    *
    * @see Random
    */
   public static MutableInfiniteInteger random(MutableInfiniteInteger nodeCount)
   {
      return random(nodeCount, new Random());
   }

   /**
    * Constructs a randomly generated InfiniteInteger, uniformly distributed over
    * the range 0 to 2^(32 * {@code nodeCount}), inclusive.
    * The uniformity of the distribution assumes that a fair source of random
    * bits is provided in {@code random}. Note that the result
    * may be positive or negative.
    *
    * @param random source of randomness to be used in computing the new
    *               InfiniteInteger.
    *
    * @return NaN if {@code nodeCount} < 1 otherwise a new random number is returned.
    */
   public static MutableInfiniteInteger random(MutableInfiniteInteger nodeCount, Random random)
   {
      if (!nodeCount.isFinite()) return NaN;
      if (is(nodeCount, LESS_THAN, MutableInfiniteInteger.valueOf(1))) return NaN;

      MutableInfiniteInteger remainingNodes = nodeCount.copy();
      MutableInfiniteInteger result = new MutableInfiniteInteger(0);
      while (!remainingNodes.equals(0))
      {
         //next int covers all possible unsigned 2^32
         result.magnitudeHead.setData(random.nextInt());
         result = result.multiplyByPowerOf2(32);
         remainingNodes = remainingNodes.subtract(1);
      }
      result = result.divideByPowerOf2DropRemainder(32);  //remove trailing 0 node
      if (!result.equals(0)) result.isNegative = random.nextBoolean();  //don't allow negative 0
      return result;
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
   public static Stream<MutableInfiniteInteger> streamAllIntegers()
   {
      return Stream.iterate(new MutableInfiniteInteger(0), (MutableInfiniteInteger previous) ->
      {
         if (previous.equals(0)) return MutableInfiniteInteger.valueOf(1);
         if (previous.isNegative) return previous.copy().abs().add(1);
         return previous.copy().negate();
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
    *
    * @see #intValue()
    * @see ReadOnlyListIterator
    */
   public static ReadOnlyListIterator<MutableInfiniteInteger> iterateAllIntegers()
   {
      return new ReadOnlyListIterator<>(new ListIterator<MutableInfiniteInteger>()
      {
         private MutableInfiniteInteger nextElement = MutableInfiniteInteger.valueOf(1);

         @Override
         public boolean hasNext(){return true;}

         @Override
         public boolean hasPrevious(){return true;}

         @Override
         public MutableInfiniteInteger next()
         {
            MutableInfiniteInteger current = nextElement.copy();
            nextElement = nextElement.add(1);
            return current;
         }

         @Override
         public MutableInfiniteInteger previous()
         {
            return nextElement = nextElement.subtract(1);
         }

         @Override
         public int nextIndex()
         {
            return nextElement.intValue();
         }

         @Override
         public int previousIndex()
         {
            return nextElement.intValue() - 1;
         }

         //will be replaced by ReadOnlyListIterator to throw:
         @Override
         public void remove(){}

         @Override
         public void set(MutableInfiniteInteger e){}

         @Override
         public void add(MutableInfiniteInteger e){}
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
   public static Stream<MutableInfiniteInteger> streamFibonacciSequence()
   {
      final Iterator<MutableInfiniteInteger> iterator = new Iterator<MutableInfiniteInteger>()
      {
         private MutableInfiniteInteger previous = null;
         private MutableInfiniteInteger back2 = null;

         @Override
         public boolean hasNext(){return true;}

         @Override
         public MutableInfiniteInteger next()
         {
            MutableInfiniteInteger next;
            if (previous == null) next = MutableInfiniteInteger.valueOf(0);
            else if (back2 == null) next = MutableInfiniteInteger.valueOf(1);
            else next = previous.copy().add(back2);
            back2 = previous;
            previous = next;
            return next;
         }
      };
      return StreamSupport.stream(Spliterators.spliteratorUnknownSize(iterator, Spliterator.ORDERED | Spliterator.IMMUTABLE), false);
   }

   /**
    * Entire code: <blockquote>{@code return (float) longValue();}</blockquote>
    *
    * @see #longValue()
    */
   @Override
   public float floatValue(){return longValue();}

   /**
    * Entire code: <blockquote>{@code return (double) longValue();}</blockquote>
    *
    * @see #longValue()
    */
   @Override
   public double doubleValue(){return longValue();}
   //TODO: I can have the floating points return Infinity or NaN

   /**
    * This method returns the least significant 31 bits of the number represented by this InfiniteInteger.
    * The int is then given the same sign as this class. This is different than a narrowing cast because
    * normally the bits would be unchanged signed or otherwise but this method performs a two's complement.
    *
    * @throws ArithmeticException if this is &plusmn;&infin; or NaN
    * @see #longValue()
    */
   @Override
   public int intValue()
   {
      if (!this.isFinite()) throw new ArithmeticException(this.toString() + " can't be even partially represented as an int.");
      int intValue =
            magnitudeHead.getData().intValue() & Integer.MAX_VALUE;  //drop the sign bit (can't use Math.abs because the nodes are unsigned)
      if (isNegative) return -intValue;
      return intValue;
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
   public long longValue()
   {
      if (!this.isFinite()) throw new ArithmeticException(this.toString() + " can't be even partially represented as a long.");

      long longValue = Integer.toUnsignedLong(magnitudeHead.getData().intValue());
      if (magnitudeHead.getNext() != null)
      {
         longValue += (Integer.toUnsignedLong(magnitudeHead.getNext().getData().intValue()) << 32);
      }
      longValue &= Long.MAX_VALUE;  //drop the sign bit (can't use Math.abs because the nodes are unsigned)
      if (isNegative) return -longValue;
      return longValue;
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
   public long longValueExact()
   {
      if (!this.isFinite()) throw new ArithmeticException(this.toString() + " can't be represented as a long.");
      if (magnitudeHead.getNext() != null && magnitudeHead.getNext().getNext() != null)
         throw new ArithmeticException("This InfiniteInteger is too large to be represented as a long.");
      //if there are too many nodes then the number is too large
      if (magnitudeHead.getNext() != null && (magnitudeHead.getNext().getData().intValue() & Long.MIN_VALUE) != 0)
         throw new ArithmeticException("This InfiniteInteger is too large to be represented as a signed long.");
      //the & Min part checks that the most significant bit must be clear since it will be dropped to make the number signed
      return longValue();
   }

   @Override
   public BigInteger bigIntegerValue()
   {
      if (!this.isFinite()) throw new ArithmeticException(this.toString() + " can't be even partially represented as a BigInteger.");
      // TODO: method stubs
      return null;  //after exact is done, copy and paste the code but return the previous result instead of throwing
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
   public BigInteger bigIntegerValueExact()
   {
      if (!this.isFinite()) throw new ArithmeticException(this.toString() + " can't be represented as a BigInteger.");

      try
      {
         DequeNode<Integer> cursor = this.magnitudeHead;
         BigInteger result = BigInteger.valueOf(Integer.toUnsignedLong(cursor.getData().intValue()));
         cursor = cursor.getNext();
         while (cursor != null)
         {
            result = result.shiftLeft(32);  //TODO: unsure of math since I am unsigned
            result = result.add(BigInteger.valueOf(Integer.toUnsignedLong(cursor.getData().intValue())));
            cursor = cursor.getNext();
         }
         if (this.isNegative) return result.negate();
         return result;
      }
      catch (Throwable t)
      {
         //ArithmeticException (from 1.8 overflow) or OutOfMemoryError etc
         //before 1.8 I assume it would throw ArrayIndexOutOfBoundsException
         //result.or will not throw but result.shiftLeft might
         throw new ArithmeticException("This InfiniteInteger is too large to be represented as a BigInteger.");
      }
   }

   /**
    * Takes less than a second to calculate but making a base 10 string took more than 40 minutes then crashed (OutOfMemoryError: Java
    * heap space).
    */
   public static BigInteger calculateMaxBigInteger()
   {
      BigInteger maxValue = BigInteger.ONE.shiftLeft(Integer.MAX_VALUE - 1).subtract(BigInteger.ONE);
      maxValue = maxValue.shiftLeft(1).add(BigInteger.ONE);
      return maxValue;
   }

   /**
    * Takes 21 seconds to calculate. Takes a total of 42 seconds to display as debugging string.
    */
   public static MutableInfiniteInteger calculateMaxBigIntegerAsInfiniteInteger()
   {
      MutableInfiniteInteger bigIntMaxValue = MutableInfiniteInteger.valueOf(1).multiplyByPowerOf2(Integer.MAX_VALUE).subtract(1);
      return bigIntMaxValue;
   }

   /**
    * <p>A Googolplex is a huge number that is easy to define. It is equal to 10<sup>10<sup>100</sup></sup>.
    * This number will not fit into a BigInteger, the reason this method is defined is to show that this class
    * does allow such a number (even if the hardware does not). This calculation is correct but is guarantee to never finish
    * due to hardware limits (see below).</p>
    *
    * <p>In order to store a googolplex you would need
    * <a href="http://www.wolframalpha.com/input/?i=log2%28googolplex%29">3e100</a> bits.
    * Assuming each bit was
    * <a href="http://www.cio.com/article/2400538/data-management/ibm-smashes-moore-s-law--cuts-bit-size-to-12-atoms.html">12 atoms</a>
    * then the entire observable universe wouldn't have enough bits
    * (<a href="http://www.wolframalpha.com/input/?i=%28atoms+in+the+observable+universe%29%2F12">8e78</a>).
    * So even though this class allows such a number the universe doesn't: but it's the thought that counts.
    * </p>
    *
    * <p>A googol on the other hand would only take 42 bytes (336 bits).</p>
    *
    * @return 10^(10^100)
    *
    * @deprecated Seriously. Don't call this method. See description.
    */
   @Deprecated
   public static MutableInfiniteInteger calculateGoogolplex()
   {
      MutableInfiniteInteger googol = MutableInfiniteInteger.valueOf(10).power(100);
      return MutableInfiniteInteger.valueOf(10).power(googol);
   }

   /**
    * http://googology.wikia.com/wiki/Arrow_notation
    * http://mathworld.wolfram.com/KnuthUp-ArrowNotation.html
    * http://mathworld.wolfram.com/KnuthUp-ArrowNotation.html
    * private: use power instead of this method
    */
   private static MutableInfiniteInteger arrowNotation(MutableInfiniteInteger a, MutableInfiniteInteger n, MutableInfiniteInteger b)
   {
      if (n.equals(1)) return a.copy().power(b);
      if (b.equals(1)) return a.copy();
      return arrowNotation(a.copy(), n.copy().subtract(1), arrowNotation(a.copy(), n.copy(), b.copy().subtract(1)));
   }

   /**
    * http://googology.wikia.com/wiki/Graham's_number
    * this function doesn't have an official name
    * private: has no legitimate use case
    */
   private static MutableInfiniteInteger grahamFunction(MutableInfiniteInteger k)
   {
      if (k.equals(0)) return new MutableInfiniteInteger(4);
      return arrowNotation(new MutableInfiniteInteger(3), grahamFunction(k.copy().subtract(1)), new MutableInfiniteInteger(3));
   }

   /**
    * Graham's Number is comically large but <a href="http://waitbutwhy.com/2014/11/1000000-grahams-number.html">hard</a>
    * <a href="http://en.wikipedia.org/wiki/Graham's_number#Definition">to</a>
    * <a href="http://googology.wikia.com/wiki/Graham's_number">define</a>.
    * This number will not fit into a BigInteger, the reason this method is defined is to show that this class
    * does allow such a number (even if the hardware does not). This calculation is correct but is guarantee to never finish
    * due to hardware limits (see Googolplex).
    *
    * @return Graham's Number which is g(64)
    *
    * @see #calculateGoogolplex()
    * @deprecated The observable universe can't store a googolplex. How many universe's worth of atoms would
    * you need for Graham's Number?
    */
   @Deprecated
   public static MutableInfiniteInteger calculateGrahamsnumber()
   {
      return grahamFunction(new MutableInfiniteInteger(64));
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
   public ReadOnlyListIterator<Integer> magnitudeIterator()
   {
      if (!this.isFinite()) throw new UnsupportedOperationException(this.toString() + " does not have nodes.");
      return new ReadOnlyListIterator<Integer>(new DequeNodeIterator.IndexAgnosticValueIterator<Integer>(magnitudeHead));
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
   public Stream<Integer> magnitudeStream()
   {
      return StreamSupport.stream(Spliterators.spliteratorUnknownSize(magnitudeIterator(), Spliterator.ORDERED | Spliterator.IMMUTABLE),
            false);
   }

   /**
    * Helper method to get the last (most significant) node of this InfiniteInteger.
    *
    * @throws NullPointerException if magnitudeHead is null
    */
   @Override
   protected DequeNode<Integer> getMagnitudeTail()
   {
      //TODO: make a variable for magnitudeTail?
      DequeNode<Integer> tail = magnitudeHead;
      while (tail.getNext() != null){ tail = tail.getNext(); }
      return tail;
   }

   /**
    * Returns an InfiniteInteger whose value is {@code (this + value)}.
    * Note that the formula used is designed for a long and is slightly more efficient
    * than calling add(InfiniteInteger.valueOf(value)) would be.
    *
    * @param value the operand to be added to this InfiniteInteger.
    *
    * @return the result including &plusmn;&infin; and NaN
    *
    * @see #add(MutableInfiniteInteger)
    */
   @Override
   public MutableInfiniteInteger add(long value)
   {
      if (!this.isFinite() || value == 0) return this;
      if (this.equals(0)) return set(MutableInfiniteInteger.valueOf(value));
      if (value == Long.MIN_VALUE) return this.add(MutableInfiniteInteger.valueOf(value));  //special case to avoid bug

      //delegations based on the sign of each
      if (!isNegative && value < 0) return this.subtract(Math.abs(value));
      if (isNegative && value > 0) return set(MutableInfiniteInteger.valueOf(value).subtract(this.abs()));

      //the rest is for if both positive or both negative
      long sum, valueRemaining = Math.abs(value);
      ListIterator<DequeNode<Integer>> thisIterator = new DequeNodeIterator.IndexAgnosticDequeIterator<Integer>(this.magnitudeHead);
      DequeNode<Integer> thisCursor = null;
      int lowValue, highValue;
      while (thisIterator.hasNext())
      {
         thisCursor = thisIterator.next();
         //turns out (true for signed and unsigned) max long > max int * max int. (2^64-1) > ((2^32-1)*(2^32-1))
         lowValue = (int) valueRemaining;
         highValue = (int) (valueRemaining >>> 32);
         sum = Integer.toUnsignedLong(thisCursor.getData().intValue()) + Integer.toUnsignedLong(lowValue);

         thisCursor.setData((int) sum);
         sum >>>= 32;

         valueRemaining = sum + Integer.toUnsignedLong(highValue);  //TODO: make a test that proves I need to use unsigned
      }
      if (valueRemaining != 0)
      {
         //the addition's carry causes the return value to have more nodes
         lowValue = (int) valueRemaining;
         highValue = (int) (valueRemaining >>> 32);
         thisCursor = DequeNode.Factory.createNodeAfter(thisCursor, lowValue);
         thisCursor = DequeNode.Factory.createNodeAfter(thisCursor, highValue);
      }
      if (thisCursor.getData().intValue() == 0) thisCursor.remove();  //remove the last node since it is a leading 0
      //do not use else. this can occur either way
      //isNegative is already correct for positive or negative
      return this;
   }

   /**
    * Entire code: <blockquote>{@code return this.add(InfiniteInteger.valueOf(value));}</blockquote>
    *
    * @see #add(MutableInfiniteInteger)
    * @see #valueOf(BigInteger)
    */
   @Override
   public MutableInfiniteInteger add(BigInteger value){return this.add(MutableInfiniteInteger.valueOf(value));}

   /**
    * Returns an InfiniteInteger whose value is {@code (this + value)}.
    *
    * @param value the operand to be added to this InfiniteInteger.
    *
    * @return the result including &plusmn;&infin; and NaN
    *
    * @see #add(long)
    */
   @Override
   public MutableInfiniteInteger add(MutableInfiniteInteger value)
   {
      if (!this.isFinite() || value.equals(0)) return this;
      if (!value.isFinite() || this.equals(0)) return set(value.copy());

      //delegations based on the sign of each
      if (!isNegative && value.isNegative) return this.subtract(value.copy().abs());
      if (isNegative && !value.isNegative) return set(value.copy().subtract(this.abs()));

      //the rest is for if both positive or both negative
      addAbove(this.magnitudeHead, value);
      //isNegative is already correct for positive or negative
      return this;
   }

   /**
    * Used internally by the add and multiply methods. This method adds value to the starting node
    * and carries upward. It does not affect any nodes before the starting node. The starting node
    * (and above) will be mutated. This method assumes both numbers are positive (or both negative).
    *
    * @param startingNode can't be null
    * @param value        can't be null or a constant
    */
   protected static void addAbove(DequeNode<Integer> startingNode, MutableInfiniteInteger value)
   {
      long sum = 0;
      DequeNode<Integer> resultCursor = startingNode;
      ListIterator<Integer> valueMagIterator = value.magnitudeIterator();
      int lowSum, highSum;
      while (valueMagIterator.hasNext() || sum != 0)
      {
         //turns out (true for signed and unsigned) max long > max int * max int. (2^64-1) > ((2^32-1)*(2^32-1))
         lowSum = (int) sum;
         highSum = (int) (sum >>> 32);
         sum = Integer.toUnsignedLong(resultCursor.getData().intValue());
         if (valueMagIterator.hasNext()) sum += Integer.toUnsignedLong(valueMagIterator.next().intValue());
         sum += Integer.toUnsignedLong(lowSum);

         resultCursor.setData((int) sum);
         sum >>>= 32;
         sum += Integer.toUnsignedLong(highSum);

         if (resultCursor.getNext() == null) resultCursor = DequeNode.Factory.createNodeAfter(resultCursor, 0);
         else resultCursor = resultCursor.getNext();
      }
      if (resultCursor.getData().intValue() == 0) resultCursor.remove();  //remove the last node since it is a leading 0
      //isNegative is already correct for positive or negative
   }

   /**
    * Returns an InfiniteInteger whose value is {@code (this - value)}.
    * Note that the formula used is designed for a long and is slightly more efficient
    * than calling subtract(InfiniteInteger.valueOf(value)) would be.
    *
    * @param value the operand to be subtracted from this InfiniteInteger.
    *
    * @return the result including &plusmn;&infin; and NaN
    *
    * @see #subtract(MutableInfiniteInteger)
    */
   @Override
   public MutableInfiniteInteger subtract(long value)
   {
      if (!this.isFinite() || value == 0) return this;
      if (value == Long.MIN_VALUE) return this.add(MutableInfiniteInteger.valueOf(value).abs());  //special case to avoid bug
      if (this.equals(0)) return set(MutableInfiniteInteger.valueOf(-value));

      //delegations based on the sign of each
      if (!isNegative && value < 0) return this.add(Math.abs(value));
      if (isNegative && value > 0) return set(this.abs().add(value).negate());
      if (isNegative && value < 0) return set(MutableInfiniteInteger.valueOf(Math.abs(value)).subtract(this.abs()));

      //the rest is for if both positive
      if (this.equals(value)) return set(new MutableInfiniteInteger(0));
      if (is(this, LESS_THAN, MutableInfiniteInteger.valueOf(value)))
         return set(MutableInfiniteInteger.valueOf(value).subtract(this).negate());

      //this is greater than value
      long difference, valueRemaining = value;
      ListIterator<DequeNode<Integer>> thisIterator = new DequeNodeIterator.IndexAgnosticDequeIterator<Integer>(this.magnitudeHead);
      DequeNode<Integer> thisCursor = null;
      int lowValue, highValue;
      boolean borrow = false;
      while (thisIterator.hasNext())
      {
         thisCursor = thisIterator.next();
         lowValue = (int) valueRemaining;
         highValue = (int) (valueRemaining >>> 32);
         difference = Integer.toUnsignedLong(thisCursor.getData().intValue()) - Integer.toUnsignedLong(lowValue);
         //Long.min is not possible so there's no bug to check
         borrow = (difference < 0);
         if (borrow) difference += Integer.toUnsignedLong((int) BitWiseUtil.HIGH_64) + 1;  //add max unsigned int +1
         //this makes difference borrow
         //the +1 is here for the same reason that when borrowing in base 10 you add 10 instead of 9

         thisCursor.setData((int) difference);
         //assert((difference >>> 32) == 0);  //due to the borrowing above

         valueRemaining = Integer.toUnsignedLong(highValue);
         if (borrow) valueRemaining++;  //subtract 1 more
      }
      //assert(valueRemaining == 0);  //because this > value
      //TODO: isn't there always a leading 0?
      if (thisCursor.getData().intValue() == 0)
      {
         thisCursor = thisCursor.getPrev();
         thisCursor.getNext().remove();
      }
      //remove the last node since it is a leading 0
      if (thisCursor.getData().intValue() == 0) thisCursor.remove();  //there will be 2 leading 0s if the last node was borrowed down to 0

      return this;
   }

   /**
    * Entire code: <blockquote>{@code return this.subtract(InfiniteInteger.valueOf(value));}</blockquote>
    *
    * @see #subtract(MutableInfiniteInteger)
    * @see #valueOf(BigInteger)
    */
   @Override
   public MutableInfiniteInteger subtract(BigInteger value){return this.subtract(MutableInfiniteInteger.valueOf(value));}

   /**
    * Returns an InfiniteInteger whose value is {@code (this - value)}.
    * Note &infin; - &infin; results in NaN.
    *
    * @param value the operand to be subtracted from this InfiniteInteger.
    *
    * @return the result including &plusmn;&infin; and NaN
    *
    * @see #subtract(long)
    */
   @Override
   public MutableInfiniteInteger subtract(MutableInfiniteInteger value)
   {
      if (this.isNaN() || value.equals(0)) return this;
      if (this.equals(0) || value.isNaN()) return set(value.copy());

      //delegations based on the sign of each
      if (!this.isNegative && value.isNegative) return this.add(value.copy().abs());
      if (this.isNegative && !value.isNegative) return set(this.abs().add(value).negate());
      if (this.isNegative && value.isNegative) return set(value.copy().abs().subtract(this.abs()));

      //the rest is for if both positive
      if (this == POSITIVE_INFINITITY && value == POSITIVE_INFINITITY) return NaN;
      if (this == POSITIVE_INFINITITY) return this;
      if (value == POSITIVE_INFINITITY) return NEGATIVE_INFINITITY;
      if (this.equals(value)) return set(new MutableInfiniteInteger(0));
      if (is(this, LESS_THAN, value)) return set(value.copy().subtract(this).negate());

      //this is greater than value
      long difference = 0;
      DequeNode<Integer> thisCursor = this.magnitudeHead;
      ListIterator<Integer> valueMagIterator = value.magnitudeIterator();
      int lowValue, highValue;
      byte borrowCount;
      while (valueMagIterator.hasNext() || difference != 0)
      {
         lowValue = (int) difference;
         highValue = (int) (difference >>> 32);
         difference = Integer.toUnsignedLong(thisCursor.getData().intValue());
         if (valueMagIterator.hasNext()) difference -= Integer.toUnsignedLong(valueMagIterator.next().intValue());
         difference -= Integer.toUnsignedLong(lowValue);
         //difference == Long.min won't cause a bug due to how borrow is programmed
         borrowCount = 0;
         while (difference < 0)  //can happen 0-2 times
         {
            borrowCount++;
            difference += Integer.toUnsignedLong((int) BitWiseUtil.HIGH_64) + 1;  //add max unsigned int +1
            //this makes difference borrow
            //the +1 is here for the same reason that when borrowing in base 10 you add 10 instead of 9
         }

         thisCursor.setData((int) difference);
         //assert((difference >>> 32) == 0);  //due to the borrowing above

         difference = Integer.toUnsignedLong(highValue) + borrowCount;  //borrow subtracts more

         if (thisCursor.getNext() != null) thisCursor = thisCursor.getNext();
         //if thisCursor is at the end then the loop is done because this > value
      }
      while (thisCursor.getData().intValue() == 0)
      {
         thisCursor = thisCursor.getPrev();
         thisCursor.getNext().remove();
      }
      //there can be any number of leading 0s

      return this;
   }

   /**
    * Returns an InfiniteInteger whose value is {@code (this * value)}.
    * Note that the formula used is designed for a long and is slightly more efficient
    * than calling multiply(InfiniteInteger.valueOf(value)) would be.
    * Note &plusmn;&infin; * 0 results in NaN.
    *
    * @param value the operand to be multiplied to this InfiniteInteger.
    *
    * @return the result including &plusmn;&infin; and NaN
    *
    * @see #multiply(MutableInfiniteInteger)
    */
   @Override
   public MutableInfiniteInteger multiply(long value)
   {
      if (value == 0 && this.isInfinite()) return NaN;
      if (!this.isFinite() || value == 1) return this;
      if (this.equals(0) || value == 0) return set(new MutableInfiniteInteger(0));
      if (value == -1) return this.negate();
      if (this.equals(1)) return set(MutableInfiniteInteger.valueOf(value));
      if (this.equals(-1)) return set(MutableInfiniteInteger.valueOf(value).negate());  //also works if value == Long.min

      boolean resultIsNegative = (isNegative != value < 0);  //!= acts as xor
      long valueRemaining = Math.abs(value);

      int lowValue = (int) valueRemaining;
      int highValue = (int) (valueRemaining >>> 32);  //this is what makes Math.abs handle Long.Min safely

      //TODO: make this mutate as it goes. also use shifting for speed
      MutableInfiniteInteger result = this.internalMultiply(lowValue);
      if (highValue != 0)
      {
         if (result.magnitudeHead.getNext() == null)
            DequeNode.Factory.createNodeAfter(result.magnitudeHead, 0);  //TODO: will this ever happen?
         addAbove(result.magnitudeHead.getNext(), this.internalMultiply(highValue));
      }
      result.isNegative = resultIsNegative;

      return set(result);
   }

   /**
    * Used internally by the multiply methods. This method multiplies this InfiniteInteger by value.
    * This method does not mutate (the returned value will be a copy) and should be removed in the future.
    * This InfiniteInteger can't be a constant or 0.
    *
    * @param value must be positive or 0
    *
    * @return the result
    */
   protected MutableInfiniteInteger internalMultiply(int value)
   {
      if (value == 0) return new MutableInfiniteInteger(0);  //this has already been compared to the singletons
      MutableInfiniteInteger result = this.copy().abs();
      DequeNode<Integer> resultCursor = result.magnitudeHead;
      long product;
      int carry = 0;
      boolean isHuge;
      while (resultCursor != null)
      {
         //max unsigned int * max unsigned int < max unsigned long but will end up being negative which makes adding carry impossible
         product = Integer.toUnsignedLong(resultCursor.getData().intValue());
         product *= Integer.toUnsignedLong(value);
         isHuge = (product < 0);
         product &= Long.MAX_VALUE;  //TODO: is Long.min possible?
         product += Integer.toUnsignedLong(carry);
         if (isHuge) product |= Long.MIN_VALUE;

         resultCursor.setData((int) product);
         product >>>= 32;
         carry = (int) product;

         resultCursor = resultCursor.getNext();
      }
      resultCursor = result.getMagnitudeTail();
      if (carry != 0) DequeNode.Factory.createNodeAfter(resultCursor, carry);
      else if (resultCursor.getData().intValue() == 0) resultCursor.remove();  //remove leading 0
      return result;
   }

   /**
    * Entire code: <blockquote>{@code return this.multiply(InfiniteInteger.valueOf(value));}</blockquote>
    *
    * @see #multiply(MutableInfiniteInteger)
    * @see #valueOf(BigInteger)
    */
   @Override
   public MutableInfiniteInteger multiply(BigInteger value){return this.multiply(MutableInfiniteInteger.valueOf(value));}

   /**
    * Returns an InfiniteInteger whose value is {@code (this * value)}.
    * Note &plusmn;&infin; * 0 results in NaN.
    *
    * @param value the operand to be multiplied to this InfiniteInteger.
    *
    * @return the result including &plusmn;&infin; and NaN
    *
    * @see #multiply(long)
    */
   @Override
   public MutableInfiniteInteger multiply(MutableInfiniteInteger value)
   {
      if (value.equals(0) && this.isInfinite()) return NaN;
      if (this.equals(0) && value.isInfinite()) return NaN;
      if (!this.isFinite() || value.equals(1)) return this;
      if (!value.isFinite() || this.equals(1)) return set(value.copy());
      if (this.equals(0) || value.equals(0)) return set(new MutableInfiniteInteger(0));
      if (value.equals(-1)) return this.negate();
      if (this.equals(-1)) return set(value.copy().negate());

      boolean resultIsNegative = (isNegative != value.isNegative);  //!= acts as xor
      MutableInfiniteInteger valueRemaining = value.copy();  //.abs() is not needed since the nodes are unsigned
      MutableInfiniteInteger result = new MutableInfiniteInteger(0);
      DequeNode<Integer> resultCursor = result.magnitudeHead;

      while (!valueRemaining.equals(0))
      {
         addAbove(resultCursor, this.internalMultiply(valueRemaining.magnitudeHead.getData().intValue()));
         valueRemaining = valueRemaining.divideByPowerOf2DropRemainder(32);
         if (resultCursor.getNext() == null) resultCursor = DequeNode.Factory.createNodeAfter(resultCursor, 0);
         else resultCursor = resultCursor.getNext();
      }
      result.isNegative = resultIsNegative;
      resultCursor = result.getMagnitudeTail();
      while (resultCursor.getData().intValue() == 0)  //remove leading 0s
      {
         resultCursor = resultCursor.getPrev();  //prev is never null because the result is not 0
         resultCursor.getNext().remove();
      }
      //TODO: make it suck less by mutating as it goes. also use shifting for speed

      return set(result);
   }

   /**
    * This method delegates because the formula used is exactly the same.
    * Entire code: <blockquote>{@code return this.multiplyByPowerOf2(InfiniteInteger.valueOf(exponent));}</blockquote>
    *
    * @see #multiplyByPowerOf2(MutableInfiniteInteger)
    * @see #valueOf(long)
    */
   @Override
   public MutableInfiniteInteger multiplyByPowerOf2(long exponent)
   {
      return this.multiplyByPowerOf2(MutableInfiniteInteger.valueOf(exponent));
   }

   /**
    * Entire code: <blockquote>{@code return this.multiplyByPowerOf2(InfiniteInteger.valueOf(exponent));}</blockquote>
    *
    * @see #multiplyByPowerOf2(MutableInfiniteInteger)
    * @see #valueOf(BigInteger)
    */
   @Override
   public MutableInfiniteInteger multiplyByPowerOf2(BigInteger exponent)
   {
      return this.multiplyByPowerOf2(MutableInfiniteInteger.valueOf(exponent));
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
    * @param exponent is also the shift distance in bits
    *
    * @return the result including &plusmn;&infin; and NaN
    *
    * @see #divideByPowerOf2DropRemainder(MutableInfiniteInteger)
    */
   @Override
   public MutableInfiniteInteger multiplyByPowerOf2(MutableInfiniteInteger exponent)
   {
      if (this.equals(0) || exponent.equals(0) || !this.isFinite()) return this;
      if (exponent.isNegative) return this.divideByPowerOf2DropRemainder(exponent.copy().abs());

      MutableInfiniteInteger shiftDistanceRemaining = exponent.copy();
      while (isComparisonResult(shiftDistanceRemaining.compareTo(32), GREATER_THAN_OR_EQUAL_TO))
      {
         this.magnitudeHead = DequeNode.Factory.createNodeBefore(0, this.magnitudeHead);
         shiftDistanceRemaining = shiftDistanceRemaining.subtract(32);
      }

      final int smallShiftDistance = shiftDistanceRemaining.intValue();
      if (smallShiftDistance != 0)
      {
         DequeNode<Integer> thisCursor = this.getMagnitudeTail();
         int overflow;

         while (thisCursor != null)
         {
            overflow = BitWiseUtil.getHighestNBits(thisCursor.getData().intValue(), smallShiftDistance);
            //overflow contains what would fall off when shifting left
            overflow >>>= (32 - smallShiftDistance);
            //shift overflow right so that it appears in the least significant place of the following node
            if (overflow != 0 && thisCursor.getNext() == null) DequeNode.Factory.createNodeAfter(thisCursor, overflow);
            else if (overflow != 0) thisCursor.getNext().setData(thisCursor.getNext().getData().intValue() | overflow);

            thisCursor.setData(thisCursor.getData().intValue() << smallShiftDistance);
            thisCursor = thisCursor.getPrev();
         }
      }
      //no need to check leading 0s since it couldn't have gained any

      return this;
   }

   /**
    * This method delegates because the formula used is exactly the same.
    * Entire code: <blockquote>{@code return divide(MutableInfiniteInteger.valueOf(value));}</blockquote>
    *
    * @see #divide(MutableInfiniteInteger)
    * @see #valueOf(long)
    */
   @Override
   public IntegerQuotient<MutableInfiniteInteger> divide(long value)
   {
      return divide(MutableInfiniteInteger.valueOf(value));
   }

   /**
    * Entire code: <blockquote>{@code return this.divide(InfiniteInteger.valueOf(value));}</blockquote>
    *
    * @see #divide(MutableInfiniteInteger)
    * @see #valueOf(BigInteger)
    */
   @Override
   public IntegerQuotient<MutableInfiniteInteger> divide(BigInteger value){return this.divide(MutableInfiniteInteger.valueOf(value));}

   /**
    * Returns an IntegerQuotient with fields of the whole and remainder of {@code (this / value)}.
    * Note that &plusmn;&infin; / &plusmn;&infin; results in IntegerQuotient(NaN, NaN).
    * X / 0 results in IntegerQuotient(NaN, NaN). &plusmn;&infin; / X == IntegerQuotient(&plusmn;&infin;, NaN);
    * X / &plusmn;&infin; == IntegerQuotient(0, NaN).
    *
    * @param value the operand to divide this InfiniteInteger by.
    *
    * @return the whole result including &plusmn;&infin; and NaN and the remainder (which can be NaN but can't be &plusmn;&infin;)
    *
    * @see IntegerQuotient
    */
   @Override
   public IntegerQuotient<MutableInfiniteInteger> divide(MutableInfiniteInteger value)
   {
      if (this.isNaN() || value.isNaN() || value.equals(0)) return new IntegerQuotient<MutableInfiniteInteger>(NaN, NaN);
      if (!this.isFinite() && !value.isFinite()) return new IntegerQuotient<MutableInfiniteInteger>(NaN, NaN);
      if (!this.isFinite()) return new IntegerQuotient<MutableInfiniteInteger>(this, NaN);
      if (!value.isFinite()) return new IntegerQuotient<MutableInfiniteInteger>(new MutableInfiniteInteger(0), NaN);

      if (value.equals(1)) return new IntegerQuotient<MutableInfiniteInteger>(this.copy(), new MutableInfiniteInteger(0));
      if (value.equals(-1)) return new IntegerQuotient<MutableInfiniteInteger>(this.copy().negate(), new MutableInfiniteInteger(0));
      if (this.equals(value))
         return new IntegerQuotient<MutableInfiniteInteger>(new MutableInfiniteInteger(1), new MutableInfiniteInteger(0));

      MutableInfiniteInteger thisAbs = this.copy().abs(), valueAbs = value.copy().abs();
      //if not equal but abs is equal then answer is -1,0
      if (thisAbs.equals(valueAbs))
         return new IntegerQuotient<MutableInfiniteInteger>(new MutableInfiniteInteger(-1), new MutableInfiniteInteger(0));
      if (is(thisAbs, LESS_THAN, valueAbs) || this.equals(0))
         return new IntegerQuotient<MutableInfiniteInteger>(new MutableInfiniteInteger(0), thisAbs);

      //shift them down as much as possible
      //I can't use thisAbs.intValue because I need all 32 bits
      while (((int) thisAbs.longValue()) == 0 && ((int) valueAbs.longValue()) == 0)
      {
         //dropping a node is fast and greatly shrinks the numbers
         thisAbs = thisAbs.divideByPowerOf2DropRemainder(32);
         valueAbs = valueAbs.divideByPowerOf2DropRemainder(32);
         //reducing doesn't affect the whole answer
      }
      final int lastShift = Math.min(Integer.numberOfTrailingZeros(((int) thisAbs.longValue())),
            Integer.numberOfTrailingZeros(((int) valueAbs.longValue())));
      if (lastShift != 0)
      {
         //passing in 0 would do nothing anyway
         thisAbs = thisAbs.divideByPowerOf2DropRemainder(lastShift);
         valueAbs = valueAbs.divideByPowerOf2DropRemainder(lastShift);
      }

      final boolean resultIsNegative = (isNegative != value.isNegative);  //!= acts as xor
      //if the remaining division can be done by the floating point unit then delegate
      if (thisAbs.equals(thisAbs.longValue()) && valueAbs.equals(valueAbs.longValue()))
      {
         long whole = (thisAbs.longValue() / valueAbs.longValue());
         final long remainder = thisAbs.longValue() % valueAbs.longValue();
         if (resultIsNegative) whole *= -1;
         return new IntegerQuotient<MutableInfiniteInteger>(new MutableInfiniteInteger(whole), new MutableInfiniteInteger(remainder));
      }

      //TODO: faster? (harder): use this method as a basis for the grade school long division
      MutableInfiniteInteger lower = new MutableInfiniteInteger(2);  //above already covered the possible ways whole could be 0 and 1
      MutableInfiniteInteger difference, midway, higher, whole = null;
      higher = ComparableSugar.max(thisAbs, valueAbs);
      higher = higher.copy().divideByPowerOf2DropRemainder(1);

      if (valueAbs.equals(1)) whole = thisAbs;  //this can only occur if valueAbs was shifted down to 1 and thisAbs > Long.Max
      while (whole == null)
      {
         difference = higher.copy().subtract(lower);
         //avoid cutting small numbers in half:
         if (is(difference, LESS_THAN, MutableInfiniteInteger.valueOf(4))) break;  //jump to small loop
         difference = difference.divideByPowerOf2DropRemainder(1);
         midway = difference.add(lower);  //diff not copied because I no longer need it
         final int compareResult = midway.copy().multiply(valueAbs).compareTo(thisAbs);
         if (isComparisonResult(compareResult, EQUAL_TO)) whole = midway;
            //if midway*valueAbs > thisAbs then midway is an upper bound
         else if (isComparisonResult(compareResult, GREATER_THAN)) higher = midway;
         else lower = midway;  //if less than
      }
      //if difference < 4 then just have lower count up (max of 4 times)
      while (whole == null)
      {
         final int compareResult = lower.copy().multiply(valueAbs).compareTo(thisAbs);
         if (isComparisonResult(compareResult, EQUAL_TO)) whole = lower;
         else if (isComparisonResult(compareResult, GREATER_THAN)) whole = lower.subtract(1);
         else lower = lower.add(1);
      }

      MutableInfiniteInteger remainder = thisAbs.copy().subtract(whole.copy().multiply(valueAbs));

      if (resultIsNegative) whole = whole.negate();
      return new IntegerQuotient<MutableInfiniteInteger>(whole, remainder);
   }

   /**
    * Aka divideReturnWhole.
    * Entire code: <blockquote>{@code return set(divide(value).getWholeResult());}</blockquote>
    *
    * @see #divide(MutableInfiniteInteger)
    * @see #valueOf(long)
    */
   @Override
   public MutableInfiniteInteger divideDropRemainder(long value)
   {
      return set(divide(value).getWholeResult());
   }

   /**
    * Aka divideReturnWhole.
    * Entire code: <blockquote>{@code return this.divideDropRemainder(InfiniteInteger.valueOf(value));}</blockquote>
    *
    * @see #divideDropRemainder(MutableInfiniteInteger)
    * @see #valueOf(BigInteger)
    */
   @Override
   public MutableInfiniteInteger divideDropRemainder(BigInteger value)
   {
      return this.divideDropRemainder(MutableInfiniteInteger.valueOf(value));
   }

   /**
    * Aka divideReturnWhole.
    * Entire code: <blockquote>{@code return set(divide(value).getWholeResult());}</blockquote>
    *
    * @see #divide(MutableInfiniteInteger)
    */
   @Override
   public MutableInfiniteInteger divideDropRemainder(MutableInfiniteInteger value)
   {
      return set(divide(value).getWholeResult());
   }

   /**
    * This method delegates because the formula used is exactly the same.
    * Entire code: <blockquote>{@code return this.divideByPowerOf2DropRemainder(InfiniteInteger.valueOf(exponent));}</blockquote>
    *
    * @see #divideByPowerOf2DropRemainder(MutableInfiniteInteger)
    * @see #valueOf(long)
    */
   @Override
   public MutableInfiniteInteger divideByPowerOf2DropRemainder(long exponent)
   {
      return divideByPowerOf2DropRemainder(MutableInfiniteInteger.valueOf(exponent));
   }

   /**
    * Entire code: <blockquote>{@code return this.divideByPowerOf2DropRemainder(InfiniteInteger.valueOf(exponent));}</blockquote>
    *
    * @see #divideByPowerOf2DropRemainder(MutableInfiniteInteger)
    * @see #valueOf(BigInteger)
    */
   @Override
   public MutableInfiniteInteger divideByPowerOf2DropRemainder(BigInteger exponent)
   {
      return divideByPowerOf2DropRemainder(MutableInfiniteInteger.valueOf(exponent));
   }

   /**
    * <p>Returns an InfiniteInteger whose value is {@code (this >>> exponent)}.
    * If the exponent is negative then a left shift is performed instead.
    * Computes <tt>truncate(this / 2<sup>exponent</sup>)</tt>.
    * Note that the nodes are unsigned and this operation ignores sign.
    * Also note that truncation occurs which means the low numbers are thrown away not rounded.
    * Therefore this operation always 0 fills and won't change the sign (unless the result is 0).</p>
    * <p>Examples:<br /><code>
    * InfiniteInteger.valueOf(-10).divideByPowerOf2DropRemainder(1) is -5<br />
    * InfiniteInteger.valueOf(100).divideByPowerOf2DropRemainder(2) is 25<br />
    * InfiniteInteger.valueOf(3).divideByPowerOf2DropRemainder(1) is 1</code></p>
    *
    * <p>This method is not named shiftRight because the direction right only makes sense for big endian numbers.</p>
    *
    * @param exponent is also the shift distance in bits
    *
    * @return the result including &plusmn;&infin; and NaN
    *
    * @see #multiplyByPowerOf2(MutableInfiniteInteger)
    */
   @Override
   public MutableInfiniteInteger divideByPowerOf2DropRemainder(MutableInfiniteInteger exponent)
   {
      if (this.equals(0) || exponent.equals(0) || !this.isFinite()) return this;
      if (exponent.isNegative) return this.multiplyByPowerOf2(exponent.copy().abs());

      MutableInfiniteInteger shiftDistanceRemaining = exponent.copy();
      while (isComparisonResult(shiftDistanceRemaining.compareTo(32), GREATER_THAN_OR_EQUAL_TO))
      {
         this.magnitudeHead = this.magnitudeHead.getNext();
         if (this.magnitudeHead == null) return set(new MutableInfiniteInteger(0));
         this.magnitudeHead.getPrev().remove();
         shiftDistanceRemaining = shiftDistanceRemaining.subtract(32);
      }

      final int smallShiftDistance = shiftDistanceRemaining.intValue();
      if (smallShiftDistance != 0)
      {
         DequeNode<Integer> thisCursor = this.magnitudeHead;
         int underflow;

         while (thisCursor.getNext() != null)
         {
            thisCursor.setData(thisCursor.getData().intValue() >>> smallShiftDistance);
            underflow = (int) BitWiseUtil.getLowestNBits(thisCursor.getNext().getData().intValue(), smallShiftDistance);
            //underflow contains what would fall off when shifting right
            underflow <<= (32 - smallShiftDistance);
            //shift underflow left so that it appears in the most significant place of the previous node
            if (underflow != 0) thisCursor.setData(thisCursor.getData().intValue() | underflow);
            thisCursor = thisCursor.getNext();
         }
         //last node simply shifts
         thisCursor.setData(thisCursor.getData().intValue() >>> smallShiftDistance);
      }

      return this;
   }

   /**
    * <p>Similar to {@code this % value} except the result is always positive. Aka: modulo, modulus, divideDropWhole, remainder.</p>
    * <p>Entire code: <blockquote>{@code return set(divide(value).getRemainder());}</blockquote></p>
    *
    * @see #divide(long)
    */
   @Override
   public MutableInfiniteInteger divideReturnRemainder(long value)
   {
      return set(divide(value).getRemainder());
   }

   /**
    * <p>Similar to {@code this % value} except the result is always positive. Aka: modulo, modulus, divideDropWhole, remainder.</p>
    * <p>Entire code: <blockquote>{@code return this.divideReturnRemainder(MutableInfiniteInteger.valueOf(value));}</blockquote></p>
    *
    * @see #divideReturnRemainder(MutableInfiniteInteger)
    * @see #valueOf(BigInteger)
    */
   @Override
   public MutableInfiniteInteger divideReturnRemainder(BigInteger value)
   {
      return this.divideReturnRemainder(MutableInfiniteInteger.valueOf(value));
   }

   /**
    * <p>Similar to {@code this % value} except the result is always positive. Aka: modulo, modulus, divideDropWhole, remainder.</p>
    * <p>Entire code: <blockquote>{@code return set(divide(value).getRemainder());}</blockquote></p>
    *
    * @see #divide(MutableInfiniteInteger)
    */
   @Override
   public MutableInfiniteInteger divideReturnRemainder(MutableInfiniteInteger value)
   {
      return set(divide(value).getRemainder());
   }

   /**
    * This method delegates because the formula used is exactly the same.
    * Entire code: <blockquote>{@code return power(MutableInfiniteInteger.valueOf(exponent));}</blockquote>
    *
    * @see #power(MutableInfiniteInteger)
    * @see #valueOf(long)
    */
   @Override
   public MutableInfiniteInteger power(long exponent)
   {
      return power(MutableInfiniteInteger.valueOf(exponent));
   }

   /**
    * Entire code: <blockquote>{@code return this.pow(InfiniteInteger.valueOf(exponent));}</blockquote>
    *
    * @see #power(MutableInfiniteInteger)
    * @see #valueOf(BigInteger)
    */
   @Override
   public MutableInfiniteInteger power(BigInteger exponent){return this.power(MutableInfiniteInteger.valueOf(exponent));}

   /**
    * Returns an InfiniteInteger whose value is this<sup>exponent</sup>.
    * There are many special cases, for a full table see {@link InfiniteInteger#powerSpecialLookUp(InfiniteInteger, InfiniteInteger) this
    * table}
    * except the pow method will return the result instead of null.
    *
    * @param exponent to which this InfiniteInteger is to be raised.
    *
    * @return the result including &plusmn;&infin; and NaN
    *
    * @throws ArithmeticException if the result would be a fraction (only possible if exponent is negative)
    */
   @Override
   public MutableInfiniteInteger power(MutableInfiniteInteger exponent)
   {
      InfiniteInteger tableValue = InfiniteInteger.powerSpecialLookUp(InfiniteInteger.valueOf(this), InfiniteInteger.valueOf(exponent));
      if (tableValue != null) return set(tableValue.toMutableInfiniteInteger());

      if (exponent.isNegative)
         throw new ArithmeticException("A negative exponent would result in a non-integer answer. The exponent was: " + exponent);
      if (this.equals(2)) return set(MutableInfiniteInteger.valueOf(1).multiplyByPowerOf2(exponent));

      //TODO: study BigInt's pow and copy it
      //but BigInt's div and gcd are too complicated
      MutableInfiniteInteger result = this.copy();
      MutableInfiniteInteger exponentRemaining = exponent.copy().subtract(1);  //already have the first time
      while (!exponentRemaining.equals(0))
      {
         result = result.multiply(this);
         exponentRemaining = exponentRemaining.subtract(1);
      }
      return set(result);
   }

   /**
    * Returns an InfiniteInteger whose value is this<sup>this</sup>.
    * This method exists mostly as a testimony that this class really can hold any integer.
    * The result will be much larger than a factorial so it will be a slow execution.
    * For example if this InfiniteInteger is 3 then 3<sup>3</sup> is 27.
    *
    * @return the result including +&infin; and NaN
    *
    * @see #power(MutableInfiniteInteger)
    */
   @Override
   public MutableInfiniteInteger selfPower(){return this.power(this.copy());}  //param must copy so that it doesn't change during calc

   /**
    * Returns an InfiniteInteger whose value is this!.
    * This method exists mostly as a testimony that this class really can hold any integer.
    * Factorial is defined as a number multiplied by all positive integers less than it.
    * So 4! = 4*3*2*1. The special cases of 1! and 0! are 1 but factorial is not defined for
    * negative numbers. If this InfiniteInteger is negative then NaN is returned.
    *
    * @return the result including +&infin; and NaN
    *
    * @see #power(MutableInfiniteInteger)
    */
   @Override
   public MutableInfiniteInteger factorial()
   {
      if (this.isNegative || this == NaN) return NaN;  //factorial is not defined for negative numbers
      if (this == POSITIVE_INFINITITY) return this;  //-Infinity is covered above
      if (this.equals(0) || this.equals(1)) return set(MutableInfiniteInteger.valueOf(1));

      MutableInfiniteInteger result = this;
      MutableInfiniteInteger integerCursor = this.copy().subtract(1);
      while (!integerCursor.equals(1))  //don't bother multiplying by 1
      {
         result = result.multiply(integerCursor);
         integerCursor = integerCursor.subtract(1);
      }
      return set(result);
   }

   /**
    * @return true if this InfiniteInteger is prime, false if it is composite
    *
    * @throws ArithmeticException if this is neither prime nor composite
    */
   public boolean isPrime()
   {
      if (this.isNegative || !this.isFinite()) throw new ArithmeticException("Prime is only defined for integers > 1 and 0");
      if (this.equals(0)) return false;
      if (this.equals(1)) throw new ArithmeticException("1 is neither prime nor composite (primality is not defined for 1)");
      if (this.equals(2)) return true;
      if (BitWiseUtil.isEven(this.intValue())) return false;

      //for everything else use an alternate version of Fermat's little theorem
      //pros: simple, never wrong, requires only 1 trial division, the power of 2 is easy for computers,
      //and requires much less memory than a sieve
      //con: testValue is huge and takes forever to divide
      final MutableInfiniteInteger less = this.copy().subtract(1);
      final MutableInfiniteInteger testValue = MutableInfiniteInteger.valueOf(1).multiplyByPowerOf2(less).subtract(1);
      return testValue.divideReturnRemainder(this).equals(0);
   }

   /**
    * This method delegates because the formula used is exactly the same.
    * Entire code: <blockquote>{@code return greatestCommonDivisor(MutableInfiniteInteger.valueOf(otherValue));}</blockquote>
    *
    * @see #greatestCommonDivisor(MutableInfiniteInteger)
    * @see #valueOf(long)
    */
   public MutableInfiniteInteger greatestCommonDivisor(long otherValue)
   {
      return greatestCommonDivisor(MutableInfiniteInteger.valueOf(otherValue));
   }

   /**
    * Entire code: <blockquote>{@code return greatestCommonDivisor(MutableInfiniteInteger.valueOf(otherValue));}</blockquote>
    *
    * @see #greatestCommonDivisor(MutableInfiniteInteger)
    * @see #valueOf(BigInteger)
    */
   public MutableInfiniteInteger greatestCommonDivisor(BigInteger otherValue)
   {
      return greatestCommonDivisor(MutableInfiniteInteger.valueOf(otherValue));
   }

   /**
    * Returns an InfiniteInteger whose value is the <a href="http://en.wikipedia.org/wiki/Greatest_common_divisor">greatest common
    * divisor</a> of
    * {@code this.abs()} and {@code otherValue.abs()}. Returns +&infin; if
    * {@code this == 0 && otherValue == 0}. Returns NaN if either is not finite.
    *
    * @param otherValue value with which the GCD is to be computed.
    *
    * @return {@code GCD(abs(this), abs(otherValue))}
    */
   public MutableInfiniteInteger greatestCommonDivisor(MutableInfiniteInteger otherValue)
   {
      MutableInfiniteInteger thisRemaining = this.copy().abs(), otherRemaining = otherValue.copy().abs();

      if (!thisRemaining.isFinite() || !otherRemaining.isFinite()) return NaN;
      if (thisRemaining.equals(otherRemaining)) return thisRemaining.copy();
      if (thisRemaining.equals(1) || otherRemaining.equals(1)) return MutableInfiniteInteger.valueOf(1);
      if (thisRemaining.equals(0) && otherRemaining.equals(0)) return POSITIVE_INFINITITY;
      if (thisRemaining.equals(0)) return otherRemaining.copy();
      if (otherRemaining.equals(0)) return thisRemaining.copy();

      MutableInfiniteInteger lower = thisRemaining, greater = otherRemaining;
      if (is(this.copy().abs(), GREATER_THAN, otherRemaining.copy().abs()))
      {
         lower = otherRemaining;
         greater = thisRemaining;
      }
      //if the lower is a factor of the greater
      if (greater.copy().divideReturnRemainder(lower).equals(0)) return lower.copy();

      MutableInfiniteInteger thisSqrt = thisRemaining.sqrtCeil(), otherSqrt = otherRemaining.sqrtCeil();
      MutableInfiniteInteger divisor = new MutableInfiniteInteger(1);

      //I can't use intValue because I need all 32 bits
      //while each have a trailing 0 digit
      while (((int) thisRemaining.longValue()) == 0 && ((int) otherRemaining.longValue()) == 0)
      {
         thisRemaining = thisRemaining.divideByPowerOf2DropRemainder(32);
         otherRemaining = otherRemaining.divideByPowerOf2DropRemainder(32);
         divisor = divisor.multiplyByPowerOf2(32);
      }
      //I can use intValue because I only need 31 bits (really I only need 1 bit)
      while (BitWiseUtil.isEven(thisRemaining.intValue()) && BitWiseUtil.isEven(otherRemaining.intValue()))
      {
         thisRemaining = thisRemaining.divideByPowerOf2DropRemainder(1);
         otherRemaining = otherRemaining.divideByPowerOf2DropRemainder(1);
         divisor = divisor.multiplyByPowerOf2(1);
      }
      //that covers the fast cases

      MutableInfiniteInteger possibleDivisor = new MutableInfiniteInteger(3);
      while (true)
      {
         //there are no divisors that are > sqrt. likewise for > remaining
         //the sqrt is of the originals and not recalculated for the remaining ones which is why I need to check both
         //it might not be valid to recalculate the sqrt each time which is why I don't
         if (is(possibleDivisor, GREATER_THAN, thisSqrt) || is(possibleDivisor, GREATER_THAN, thisRemaining)) break;
         if (is(possibleDivisor, GREATER_THAN, otherSqrt) || is(possibleDivisor, GREATER_THAN, otherRemaining)) break;

         //if they both can divide without remainder
         if (thisRemaining.copy().divideReturnRemainder(possibleDivisor).equals(0) && otherRemaining.copy()
                                                                                                    .divideReturnRemainder(possibleDivisor)
                                                                                                    .equals(0))
         {
            thisRemaining = thisRemaining.divideDropRemainder(possibleDivisor);
            otherRemaining = otherRemaining.divideDropRemainder(possibleDivisor);
            divisor = divisor.multiply(possibleDivisor);
            //do not increase possibleDivisor because it might divide multiple times
         }
         else possibleDivisor = possibleDivisor.add(2);  //every odd since I know they are no even factors left
      }

      return divisor;
   }

   /**
    * This method finds the ceiling of the square root of this InfiniteInteger.
    * For example if the actual square root is 4.1 the returned value will be 5,
    * if the number is 25 then 5 is returned etc.
    * If +&infin; is passed in then +&infin; is returned.
    * This method does not mutate and the returned value will be a copy.
    *
    * @return an upper bound for the square root. NaN is returned if this is negative or NaN.
    *
    * @see Math#ceil(double)
    */
   protected MutableInfiniteInteger sqrtCeil()
   {
      if (this.isNaN() || this.isNegative) return NaN;
      if (this == POSITIVE_INFINITITY) return this;

      //if fits into signed long then delegate
      if (this.equals(this.longValue()))
      {
         return MutableInfiniteInteger.valueOf(((long) Math.ceil(Math.sqrt(this.longValue()))));
      }

      MutableInfiniteInteger higher = this.estimateSqrt();
      if (higher.copy().power(2).equals(this)) return higher;  //if already the exact answer
      MutableInfiniteInteger lower = higher.copy().divideByPowerOf2DropRemainder(1);
      MutableInfiniteInteger difference, midway;

      while (true)
      {
         difference = higher.copy().subtract(lower);
         //avoid cutting small numbers in half:
         if (is(difference, LESS_THAN, MutableInfiniteInteger.valueOf(4))) break;
         difference = difference.divideByPowerOf2DropRemainder(1);
         midway = difference.add(lower);  //diff not copied because I no longer need it
         int compareResult = midway.copy().power(2).compareTo(this);
         if (isComparisonResult(compareResult, EQUAL_TO)) return midway;
         //if midway^2 > this then midway is an upper bound for the sqrt
         if (isComparisonResult(compareResult, GREATER_THAN)) higher = midway;
         else lower = midway;
      }
      //if difference < 4 then just have lower count up (max of 3 times)
      while (true)
      {
         if (is(lower.copy().power(2), GREATER_THAN_OR_EQUAL_TO, this)) return lower;
         lower = lower.add(1);
      }
      //unreachable
   }

   /**
    * This method is a fast high estimation of the square root of this InfiniteInteger.
    * The number returned will a power of 2 greater than or equal to the actual square root.
    * Therefore worst case senerio the estimation returned will be one less than twice the actual square root
    * (ie {@code worst_estimation == sqrt *2 -1}).
    * The value of this InfiniteInteger can't be negative and must be finite.
    * This method does not mutate and the returned value will be a copy.
    *
    * @return an upper bound for the square root
    */
   protected MutableInfiniteInteger estimateSqrt()
   {
      if (this.equals(0) || this.equals(1)) return this.copy();
      if (is(this, LESS_THAN_OR_EQUAL_TO, MutableInfiniteInteger.valueOf(4))) return MutableInfiniteInteger.valueOf(2);

      //cutting the number of digits in half works for any number base (except base 1) but base 2 has the closest estimation
      //worst case is off by 1 digit therefore base 2 has the smallest error (being off by *2)
      //base 1 would always return thisValue/2 which isn't a good estimation
      MutableInfiniteInteger binaryDigits = MutableInfiniteInteger.valueOf(0);
      DequeNode<Integer> thisCursor = this.magnitudeHead;
      while (thisCursor.getNext() != null)
      {
         binaryDigits = binaryDigits.add(32);
         thisCursor = thisCursor.getNext();
      }
      binaryDigits = binaryDigits.add(32);  //for the last node
      //counting leading 0s functions as expected for unsigned numbers
      binaryDigits = binaryDigits.subtract(Integer.numberOfLeadingZeros(thisCursor.getData().intValue()));
      //subtract the unused bits

      //I can use binaryDigits.intValue because I only need 31 bits (really I only need 1 bit)
      boolean isExact = BitWiseUtil.isOdd((binaryDigits.intValue()));
      if (isExact) binaryDigits = binaryDigits.add(1);  //make it even by rounding up
      binaryDigits = binaryDigits.divideByPowerOf2DropRemainder(1);

      MutableInfiniteInteger estimation = MutableInfiniteInteger.valueOf(1);
      while (isComparisonResult(binaryDigits.compareTo(32), GREATER_THAN_OR_EQUAL_TO))
      {
         binaryDigits = binaryDigits.subtract(32);
         estimation = estimation.multiplyByPowerOf2(32);
      }
      estimation = estimation.multiplyByPowerOf2(binaryDigits);

      if (isExact && this.isPowerOf2()) estimation = estimation.divideByPowerOf2DropRemainder(1);
      //the estimation will be the actual square root in this case

      return estimation;
   }

   /**
    * Returns the absolute value of this InfiniteInteger.
    *
    * @return itself or the positive version of this
    *
    * @see Math#abs(double)
    */
   @Override
   public MutableInfiniteInteger abs()
   {
      if (!isNegative || isNaN()) return this;  //includes this == 0 and +Infinity
      if (this == NEGATIVE_INFINITITY) return POSITIVE_INFINITITY;
      isNegative = false;
      return this;
   }

   /**
    * Returns an InfiniteInteger whose value is {@code (0-this)}.
    *
    * @return {@code -this}
    */
   @Override
   public MutableInfiniteInteger negate()
   {
      if (isNaN() || this.equals(0)) return this;  //0 is a special case because -0 == 0
      if (this == NEGATIVE_INFINITITY) return POSITIVE_INFINITITY;
      if (this == POSITIVE_INFINITITY) return NEGATIVE_INFINITITY;
      isNegative = !isNegative;
      return this;
   }

   /**
    * @return -1, 0 or 1 as the value of this number is negative, zero or
    * positive respectively. NaN returns 0.
    */
   @Override
   public byte signum()
   {
      if (isNegative) return -1;
      if (this.equals(0) || this == NaN) return 0;
      return 1;
   }

   //TODO: add min/max. maybe static (InfInt, InfInt) only?
   //big int also has bitwise operations. gcd. and weird methods

   /**
    * Compares this == NaN.
    *
    * @return true if this InfiniteInteger is the constant for NaN.
    *
    * @see #NaN
    */
   @Override
   public boolean isNaN(){return this == NaN;}

   /**
    * Compares this InfiniteInteger to both positive and negative infinity.
    *
    * @return true if this InfiniteInteger is either of the infinity constants.
    *
    * @see #POSITIVE_INFINITITY
    * @see #NEGATIVE_INFINITITY
    */
   @Override
   public boolean isInfinite(){return (this == POSITIVE_INFINITITY || this == NEGATIVE_INFINITITY);}

   /**
    * Compares this InfiniteInteger to &plusmn;&infin; and NaN (returns false if this is any of them).
    *
    * @return true if this InfiniteInteger is not a special value (ie if this is a finite number).
    *
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
   public void signalNaN(){if (isNaN()) throw new ArithmeticException("Not a number.");}

   /**
    * Returns {@code true} if <code>this.abs() == 2<sup>n</sup></code> where {@code n} is any finite non-negative integer.
    * The exception is that 0 ({@code n} is -&infin;) returns {@code true}.
    *
    * @return {@code true} if this is a power of 2 (including 0 and 1)
    *
    * @see #abs()
    */
   //this method exists because I need it for estimateSqrt(MutableInfiniteInteger)
   public boolean isPowerOf2()
   {
      if (!this.isFinite()) return false;

      DequeNode<Integer> thisCursor;
      for (thisCursor = this.magnitudeHead; thisCursor.getNext() != null; thisCursor = thisCursor.getNext())
      {
         //all nodes that aren't the most significant must be 0
         if (0 != thisCursor.getData().intValue()) return false;
      }

      //and check the most significant node
      return BitWiseUtil.isPowerOf2(thisCursor.getData().intValue());
   }

   /**
    * Compares this InfiniteInteger with the specified object for numeric equality.
    * Note that this equality is not always symmetric as other.equals(this) != this.equals(other).
    *
    * @param other the value to be compared to this
    *
    * @return true if this InfiniteInteger has the same numeric value as other. false if other is not a number
    *
    * @see #equals(MutableInfiniteInteger)
    */
   @Override
   public boolean equals(Object other)
   {
      if (other == null) return false;
      if (other instanceof MutableInfiniteInteger) return this.equals((MutableInfiniteInteger) other);  //checks this == other
      if (other instanceof BigInteger) return this.equals(MutableInfiniteInteger.valueOf((BigInteger) other));
      if (!this.isFinite()) return false;  //TODO: actually compare specials to floating point's
      //TODO: floating point can be larger than long:
      /*
       * if(double != Math.floor(double)) return false;
		 * return this.equals(InfiniteInteger.valueOf(BigDecimal.valueOf(double).toBigIntegerExact()));
		 * also use this for doubleValue and valueOf(double)
		 */
      if (other instanceof Number) return this.equals(((Number) other).longValue());  //TODO: should I use long or double value?
      return false;
   }

   /**
    * Compares this InfiniteInteger with the specified object for numeric equality.
    *
    * @param other the value to be compared to this
    *
    * @return true if this InfiniteInteger has the same numeric value as other
    *
    * @see #compareTo(MutableInfiniteInteger)
    */
   @Override
   public boolean equals(MutableInfiniteInteger other)
   {
      if (other == null) return false;
      if (this == other) return true;
      //these are singletons. if not the same object then it's not equal
      if (!this.isFinite() || !other.isFinite()) return false;
      if (isNegative != other.isNegative) return false;
      DequeNode<Integer> thisCursor = this.magnitudeHead;
      DequeNode<Integer> otherCursor = other.magnitudeHead;
      while (thisCursor != null && otherCursor != null)
      {
         if (!Objects.equals(thisCursor.getData(), otherCursor.getData())) return false;
         thisCursor = thisCursor.getNext();
         otherCursor = otherCursor.getNext();
      }
      return (thisCursor == otherCursor);  //they must both be null (at end)
   }

   /**
    * Compares this InfiniteInteger with the specified value for numeric equality.
    * Note that this equality is symmetric with Long.valueOf(value).equals(this.longValueExact())
    * only if this <= Long.MAX_VALUE.
    *
    * @param value the value to be compared to this
    *
    * @return true if this InfiniteInteger has the same numeric value as the value parameter
    *
    * @see #longValueExact()
    * @see #compareTo(long)
    */
   @Override
   public boolean equals(long value)
   {
      if (!this.isFinite()) return false;

      if (magnitudeHead.getNext() != null && magnitudeHead.getNext().getNext() != null)
         return false;  //this is larger than max unsigned long (this check does need to be made)
      if (magnitudeHead.getNext() != null && (magnitudeHead.getNext().getData().intValue() & Long.MIN_VALUE) != 0)
         return false;  //this is larger than max signed long

      return (value == this.longValue());
   }

   /**
    * Compares this InfiniteInteger with the specified other for numeric equality.
    * The natural order is as expected with &plusmn;&infin; being at either end.
    * With the exception that &infin; &lt; NaN (this is consistent with Float/Double.compareTo).
    *
    * @param other the value to be compared to this
    *
    * @return -1, 0 or 1 if this InfiniteInteger is numerically less than, equal
    * to, or greater than other.
    */
   @Override
   public int compareTo(MutableInfiniteInteger other)
   {
      if (this == other) return THIS_EQUAL;  //recall that special values are singletons
      if (this == NaN || other == NEGATIVE_INFINITITY) return THIS_GREATER;
      if (this == NEGATIVE_INFINITITY || other == NaN) return THIS_LESSER;
      //+Infinity is only greater if NaN isn't involved
      if (this == POSITIVE_INFINITITY) return THIS_GREATER;
      if (other == POSITIVE_INFINITITY) return THIS_LESSER;

      if (isNegative && !other.isNegative) return THIS_LESSER;
      if (!isNegative && other.isNegative) return THIS_GREATER;  //also covers if this.equals(0)

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
         else if (thisCursor.getNext() != null) return THIS_GREATER;
         else return THIS_LESSER;
      }

      //they have the same number of nodes and both cursors are pointing to the most significant (last) node
      int thisData, otherData;
      while (thisCursor != null)
      {
         thisData = thisCursor.getData().intValue();
         otherData = otherCursor.getData().intValue();
         if (thisData != otherData) return Integer.compareUnsigned(thisData, otherData);
         thisCursor = thisCursor.getPrev();
         otherCursor = otherCursor.getPrev();
      }

      //same length and all nodes have the same data
      return THIS_EQUAL;
   }

   /**
    * Compares this InfiniteInteger with the specified other for numeric equality.
    * Even though sorting is not possible this method returns as expected.
    * Entire code: <blockquote>{@code return this.compareTo(InfiniteInteger.valueOf(other));}</blockquote>
    *
    * @param other the value to be compared to this
    *
    * @see #compareTo(MutableInfiniteInteger)
    * @see Comparable#compareTo(Object)
    */
   @Override
   public int compareTo(BigInteger other){return this.compareTo(MutableInfiniteInteger.valueOf(other));}

   /**
    * Compares this InfiniteInteger with the specified other for numeric equality.
    * Even though sorting is not possible this method returns as expected.
    * Entire code: <blockquote>{@code return this.compareTo(InfiniteInteger.valueOf(other));}</blockquote>
    *
    * @param other the value to be compared to this
    *
    * @see #compareTo(MutableInfiniteInteger)
    * @see Comparable#compareTo(Object)
    */
   @Override
   public int compareTo(long other){return this.compareTo(MutableInfiniteInteger.valueOf(other));}

   /**
    * Returns the hash code for this InfiniteInteger.
    * Collisions are, in theory, likely when comparing all possible integers
    * with all possible values that can fit into int.
    *
    * @return hash code for this InfiniteInteger.
    */
   @Override
   public int hashCode()
   {
      if (this == NaN) return Integer.MIN_VALUE;  //so that 0 and NaN will not have a collision
      if (this.isInfinite()) return (Integer.MAX_VALUE * this.signum());  //to prevent collision with +/-1
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
   public String toString()
   {
      if (this == POSITIVE_INFINITITY) return "+Infinity";  //it doesn't seem like \u221E works
      if (this == NEGATIVE_INFINITITY) return "-Infinity";
      if (this == NaN) return "NaN";
      if (this.equals(0)) return "0";
      //method stub
      //BigInteger > string max
      //BigInteger.toString(any) doesn't check range and will just crash
      //BigInteger.toString(not small) is recursive and will run out of RAM
      //instead of messing with all that I think I'll just return the base 10 string if possible or "> 2^max+" otherwise
      //unfortunately this stores in base 2 so I don't know how to display it in base 10
      //return "2^?";
      return toDebuggingString();
   }

   private String toDebuggingString()
   {
      String stringValue = "+ ";
      if (isNegative) stringValue = "- ";

      StringBuilder stringBuilder = new StringBuilder(stringValue);
      for (DequeNode<Integer> cursor = magnitudeHead; cursor != null; cursor = cursor.getNext())
      {
         stringBuilder.append(Integer.toHexString(cursor.getData().intValue()).toUpperCase());
         stringBuilder.append(", ");
      }
      return stringBuilder.toString();
   }

   @Override
   public void toFile(File writeToHere)
   {
      // method stub. it can always fit into a file (assuming the JVM and OS had no max file size which they do)
   }
   //I could implement writeObject and readObject but the JVM default serialization works fine.
   //readObject is possible by putting a long for count of following nodes that exist and repeat
   //for example if there were Long.max+1 nodes then serialize: Long.max, all but last node, 1, last node

   /**
    * In order to maintain the singleton constants they will not be copied.
    * So &plusmn;&infin; and NaN will return themselves but all others will be copied as expected.
    *
    * @return a copy or a defined singleton
    */
   @Override
   public MutableInfiniteInteger copy()
   {
      if (!this.isFinite()) return this;
      MutableInfiniteInteger returnValue = new MutableInfiniteInteger(0);
      returnValue.isNegative = this.isNegative;
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

   /**
    * Mutates this to have the same value as the parameter
    *
    * @return the result which is itself or a defined singleton
    */
   public MutableInfiniteInteger set(long newValue)
   {
      return this.set(MutableInfiniteInteger.valueOf(newValue));
   }

   /**
    * Mutates this to have the same value as the parameter
    *
    * @return the result which is itself or a defined singleton
    */
   public MutableInfiniteInteger set(BigInteger newValue)
   {
      return this.set(MutableInfiniteInteger.valueOf(newValue));
   }

   /**
    * Mutates this to have the same value as the parameter.
    * In order to maintain the singleton constants mutation will not
    * occur is this or the parameter are a singleton constant.
    *
    * @return the result which is itself or a defined singleton
    */
   public MutableInfiniteInteger set(MutableInfiniteInteger newValue)
   {
      if (!newValue.isFinite()) return newValue;  //if this is also a constant then return the new value
      if (!this.isFinite()) return this;
      this.isNegative = newValue.isNegative;
      DequeNode<Integer> valueCursor = newValue.magnitudeHead;

      this.magnitudeHead = DequeNode.Factory.createStandAloneNode(valueCursor.getData());  //drop all other nodes of this
      DequeNode<Integer> thisCursor = this.magnitudeHead;
      valueCursor = valueCursor.getNext();  //must be outside the loop since the first node already exists
      while (valueCursor != null)
      {
         thisCursor = DequeNode.Factory.createNodeAfter(thisCursor, valueCursor.getData());
         valueCursor = valueCursor.getNext();
      }
      return this;
   }

}
