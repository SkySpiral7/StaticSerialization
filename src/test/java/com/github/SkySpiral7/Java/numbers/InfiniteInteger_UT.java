package com.github.SkySpiral7.Java.numbers;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.ListIterator;

import com.github.SkySpiral7.Java.iterators.JumpingIterator;
import org.junit.Test;

import static com.github.SkySpiral7.Java.pojo.Comparison.EQUAL_TO;
import static com.github.SkySpiral7.Java.util.ComparableSugar.is;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.lessThan;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

/**
 * Methods that simply delegate do not need a test.
 * Additionally the following do not need a test:
 * the other versions of littleEndian and bigEndian, magnitude Iterator and Stream,
 * getMagnitudeTail, selfPower, factorial, abs, negate, signum, isNaN, isInfinite, isFinite, signalNaN,
 * the other versions of equals, hashCode, toFile (and not toString because it delegates)
 */
public class InfiniteInteger_UT
{
   private InfiniteInteger infiniteInteger;
   //TODO: make test to ensure nothing is mutated

   @Test
   public void compareTo_special()
   {
      //TODO: finish test
      //don't use hamcrest for these because they would use .equals
      assertTrue(is(InfiniteInteger.ZERO, EQUAL_TO, InfiniteInteger.ZERO));

      //use hamcrest for rest to get a more meaningful failure message
      assertThat(InfiniteInteger.POSITIVE_INFINITITY, greaterThan(InfiniteInteger.ZERO));
      assertThat(InfiniteInteger.NEGATIVE_INFINITITY, lessThan(InfiniteInteger.ZERO));
   }

   @Test
   public void equals()
   {
      assertEquals(InfiniteInteger.valueOf(10), InfiniteInteger.valueOf(10));
      assertEquals(InfiniteInteger.valueOf(5).add(5), InfiniteInteger.valueOf(7).add(3));
      infiniteInteger = InfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2);
      assertEquals(infiniteInteger, infiniteInteger);
      assertNotEquals(infiniteInteger.add(1), infiniteInteger);
   }

   @Test
   public void fastPaths()
   {
      //TODO: more fast paths but move them into each other test
      assertSame(InfiniteInteger.POSITIVE_INFINITITY, InfiniteInteger.POSITIVE_INFINITITY.add(12));
      assertSame(InfiniteInteger.NEGATIVE_INFINITITY, InfiniteInteger.NEGATIVE_INFINITITY.add(12));
      assertSame(InfiniteInteger.NaN, InfiniteInteger.NaN.add(12));
      assertSame(InfiniteInteger.ZERO, InfiniteInteger.valueOf(0));

      assertSame(InfiniteInteger.POSITIVE_INFINITITY, InfiniteInteger.POSITIVE_INFINITITY.add(BigInteger.TEN));
      assertSame(InfiniteInteger.NEGATIVE_INFINITITY, InfiniteInteger.NEGATIVE_INFINITITY.add(BigInteger.TEN));
      assertSame(InfiniteInteger.NaN, InfiniteInteger.NaN.add(BigInteger.TEN));
      assertSame(InfiniteInteger.ZERO, InfiniteInteger.valueOf(BigInteger.ZERO));

      //must use debugger to see if the fast path was used for these
      //these ones should not be moved since they are not visible
       /*
       InfiniteInteger.ZERO.add(12);
    	InfiniteInteger.valueOf(BigInteger.TEN);
    	/**/
   }

   @Test
   public void iterateAllIntegers()
   {
      ListIterator<InfiniteInteger> integerIterator = InfiniteInteger.iterateAllIntegers();
      assertSame(InfiniteInteger.ZERO, integerIterator.previous());
      integerIterator.next();
      assertEquals(InfiniteInteger.valueOf(1), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(2), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(3), integerIterator.next());
      assertTrue(integerIterator.hasNext());  //continues forever

      assertEquals(4, integerIterator.nextIndex());
      JumpingIterator.jumpByIndex(integerIterator, -4);
      assertEquals(0, integerIterator.nextIndex());
      assertEquals(-1, integerIterator.previous().intValue());
      assertEquals(-2, integerIterator.previous().intValue());
      assertEquals(-3, integerIterator.previous().intValue());
      assertTrue(integerIterator.hasPrevious());  //continues forever
   }

   @Test
   public void littleEndian()
   {
      assertSame(InfiniteInteger.ZERO, InfiniteInteger.littleEndian(Collections.emptyIterator(), true));
      Iterator<Long> input = Arrays.asList(0L, 0L, 0L, 0L, 0L).iterator();
      assertSame(InfiniteInteger.ZERO, InfiniteInteger.littleEndian(input, true));
      input = Arrays.asList(1L, 1L, Long.MAX_VALUE, 0L).iterator();
      assertEqualNodes(InfiniteInteger.littleEndian(input, true), -1, 1, 0, 1, 0, -1, Integer.MAX_VALUE);
   }

   @Test
   public void streamAllIntegers()
   {
      Iterator<InfiniteInteger> integerIterator = InfiniteInteger.streamAllIntegers().iterator();
      assertSame(InfiniteInteger.ZERO, integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(1), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(-1), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(2), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(-2), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(3), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(-3), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(4), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(-4), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(5), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(-5), integerIterator.next());
      assertTrue(integerIterator.hasNext());  //continues forever
   }

   @Test
   public void streamFibonacciSequence()
   {
      Iterator<InfiniteInteger> integerIterator = InfiniteInteger.streamFibonacciSequence().iterator();
      assertSame(InfiniteInteger.ZERO, integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(1), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(1), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(2), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(3), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(5), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(8), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(13), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(21), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(34), integerIterator.next());
      assertEquals(InfiniteInteger.valueOf(55), integerIterator.next());
      assertTrue(integerIterator.hasNext());  //continues forever
   }

   @Test
   public void valueOf_BigInteger()
   {
      assertEquals(InfiniteInteger.valueOf(5), InfiniteInteger.valueOf(BigInteger.valueOf(5)));
      assertEquals(InfiniteInteger.valueOf(Long.MAX_VALUE - 5), InfiniteInteger.valueOf(BigInteger.valueOf(Long.MAX_VALUE - 5)));

      infiniteInteger = InfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2).negate();
      BigInteger input = BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.valueOf(Long.MAX_VALUE)).add(BigInteger.valueOf(2)).negate();
      assertEquals(infiniteInteger, InfiniteInteger.valueOf(input));
   }

   @Test
   public void valueOf_long()
   {
      //simple case
      assertEqualNodes(InfiniteInteger.valueOf(-5), -1, 5);

      //more than max int
      assertEqualNodes(InfiniteInteger.valueOf(8_589_934_592L), 1, 0, 2);

      //prove that each node is unsigned
      assertEqualNodes(InfiniteInteger.valueOf(2_147_483_648L), 1, (int) 2_147_483_648L);

      //special case: can't use Math.abs
      assertEqualNodes(InfiniteInteger.valueOf(Long.MIN_VALUE), -1, 0, Integer.MIN_VALUE);
   }

   private void assertEqualNodes(InfiniteInteger infiniteIntegerParam, int expectedSignum, int... expectedNodes)
   {
      assertEquals(generateInfiniteIntegerString(expectedSignum, expectedNodes), infiniteIntegerParam.toString());
   }

   //tightly coupled with the current debugging InfiniteInteger.toString()
   private String generateInfiniteIntegerString(int signum, int... nodeValues)
   {
      if (signum == 0) return "0";
      String returnValue = "+ ";
      if (signum == -1) returnValue = "- ";
      StringBuilder stringBuilder = new StringBuilder(returnValue);
      for (int node : nodeValues)
      {
         stringBuilder.append(Integer.toHexString(node).toUpperCase());
         stringBuilder.append(", ");
      }
      return stringBuilder.toString();
   }

}
