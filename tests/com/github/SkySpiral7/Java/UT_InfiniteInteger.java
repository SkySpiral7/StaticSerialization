package com.github.SkySpiral7.Java;

import static com.github.SkySpiral7.Java.pojo.Comparison.EQUAL_TO;
import static com.github.SkySpiral7.Java.util.ComparableSugar.is;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.lessThan;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Methods that simply delegate do not need a test.
 * Additionally the following do not need a test:
 * the other versions of littleEndian and bigEndian, magnitude Iterator and Stream,
 * getMagnitudeTail, selfPower, factorial, abs, negate, signum, isNaN, isInfinite, isFinite, signalNaN,
 * the other versions of equals, hashCode, copy, toFile (but toString should be tested when finished)
 */
public class UT_InfiniteInteger {
    private InfiniteInteger infiniteInteger;

    @Test
    public void add_long() {
    	//simple case
    	assertEqualNodes(InfiniteInteger.valueOf(5).add(5), 1, 10);

    	//more than max int
    	assertEqualNodes(InfiniteInteger.valueOf(8_589_934_592L).add(5), 1, 5, 2);

    	//more than max long
    	assertEqualNodes(InfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2), 1, 0, 0, 1);

    	//special case is negative but can't use Math.abs
    	assertEqualNodes(InfiniteInteger.valueOf(-1).add(Long.MIN_VALUE), -1, 1, Integer.MIN_VALUE);
    }

    @Test
    public void add_InfiniteInteger() {
    	//simple case
    	assertEqualNodes(InfiniteInteger.valueOf(5).add(InfiniteInteger.valueOf(5)), 1, 10);

    	//more than max int
    	assertEqualNodes(InfiniteInteger.valueOf(8_589_934_592L).add(InfiniteInteger.valueOf(5)), 1, 5, 2);

    	//more than max long
    	infiniteInteger = InfiniteInteger.valueOf(Long.MAX_VALUE).add(InfiniteInteger.valueOf(Long.MAX_VALUE)).add(InfiniteInteger.valueOf(2));
    	assertEqualNodes(infiniteInteger, 1, 0, 0, 1);
    }

    @Test
    @Ignore
    public void bigIntegerValueExact() {
    	//TODO: not tested
    	//it should not test the max size as that takes way too long. Although putting it in another ignored test is fine
    }

    @Test
    public void compareTo() {
    	//don't use hamcrest for these because they would use .equals
    	infiniteInteger = InfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2);
    	assertTrue(is(infiniteInteger, EQUAL_TO, infiniteInteger));  //same object
    	assertTrue(is(infiniteInteger.copy(), EQUAL_TO, infiniteInteger));  //different object same value

    	//use hamcrest for rest to get a more meaningful failure message
    	assertThat(InfiniteInteger.valueOf(-5), lessThan(InfiniteInteger.valueOf(5)));
    	assertThat(InfiniteInteger.valueOf(5), greaterThan(InfiniteInteger.valueOf(-5)));
    	assertThat(InfiniteInteger.valueOf(10), greaterThan(InfiniteInteger.valueOf(5)));
    	assertThat(infiniteInteger, greaterThan(InfiniteInteger.valueOf(10)));  //left has more nodes
    	assertThat(infiniteInteger.add(1), greaterThan(infiniteInteger));  //same node count but different value
    	infiniteInteger = InfiniteInteger.valueOf(Integer.MAX_VALUE).add(1);
    	assertThat(infiniteInteger.add(1), greaterThan(infiniteInteger));  //make sure nodes are compared unsigned
    }

    @Test
    public void compareTo_special() {
    	//TODO: finish test
    	//don't use hamcrest for these because they would use .equals
    	assertTrue(is(InfiniteInteger.ZERO, EQUAL_TO, InfiniteInteger.ZERO));

    	//use hamcrest for rest to get a more meaningful failure message
    	assertThat(InfiniteInteger.POSITIVE_INFINITITY, greaterThan(InfiniteInteger.ZERO));
    	assertThat(InfiniteInteger.NEGATIVE_INFINITITY, lessThan(InfiniteInteger.ZERO));
    }

    @Test
	public void divideByPowerOf2DropRemainder() {
    	//simple case
    	assertEqualNodes(InfiniteInteger.valueOf(1024).divideByPowerOf2DropRemainder(3), 1, 128);

    	//shift by x32
    	infiniteInteger = InfiniteInteger.valueOf(1).multiplyByPowerOf2(64).add(Long.MAX_VALUE);
    	assertEqualNodes(infiniteInteger.divideByPowerOf2DropRemainder(64), 1, 1);

    	//shift more than 32
    	infiniteInteger = InfiniteInteger.valueOf(1).multiplyByPowerOf2(32*3).subtract(1);  //3 nodes all high
    	assertEqualNodes(infiniteInteger.divideByPowerOf2DropRemainder(35), 1, -1, 0x1FFF_FFFF);
    }

    @Test
    public void equals() {
    	assertEquals(InfiniteInteger.valueOf(10), InfiniteInteger.valueOf(10));
    	assertEquals(InfiniteInteger.valueOf(5).add(5), InfiniteInteger.valueOf(7).add(3));
    	infiniteInteger = InfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2);
    	assertEquals(infiniteInteger, infiniteInteger);
    	assertEquals(infiniteInteger.copy(), infiniteInteger);
    	assertNotEquals(infiniteInteger.add(1), infiniteInteger);
    }

    @Test
    public void fastPaths() {
    	//TODO: more fast paths but move them into each other test
    	assertSame(InfiniteInteger.POSITIVE_INFINITITY, InfiniteInteger.POSITIVE_INFINITITY.add(12));
    	assertSame(InfiniteInteger.NEGATIVE_INFINITITY, InfiniteInteger.NEGATIVE_INFINITITY.add(12));
    	assertSame(InfiniteInteger.NaN, InfiniteInteger.NaN.add(12));
    	assertSame(InfiniteInteger.ZERO, InfiniteInteger.valueOf(0));

    	assertSame(InfiniteInteger.POSITIVE_INFINITITY, InfiniteInteger.POSITIVE_INFINITITY.add(BigInteger.TEN));
    	assertSame(InfiniteInteger.NEGATIVE_INFINITITY, InfiniteInteger.NEGATIVE_INFINITITY.add(BigInteger.TEN));
    	assertSame(InfiniteInteger.NaN, InfiniteInteger.NaN.add(BigInteger.TEN));
    	assertSame(InfiniteInteger.ZERO, InfiniteInteger.valueOf(BigInteger.ZERO));

    	infiniteInteger = InfiniteInteger.valueOf(12);
    	assertSame(infiniteInteger, infiniteInteger.add(0));
    	assertSame(infiniteInteger, infiniteInteger.add(InfiniteInteger.ZERO));

    	//must use debugger to see if the fast path was used for these
    	//these ones should not be moved since they are not visible
    	/*
    	InfiniteInteger.ZERO.add(12);
    	InfiniteInteger.valueOf(BigInteger.TEN);
    	/**/
    }

    @Test
    public void intValue() {
    	assertEquals(5, InfiniteInteger.valueOf(5).intValue());
    	assertEquals(Integer.MAX_VALUE, InfiniteInteger.valueOf(Integer.MAX_VALUE).intValue());
    	infiniteInteger = InfiniteInteger.valueOf(Integer.MAX_VALUE).add(Integer.MAX_VALUE).add(1);
    	assertEquals(Integer.MAX_VALUE, infiniteInteger.intValue());

    	assertEquals(-1, InfiniteInteger.valueOf(-1).intValue());
    	assertEquals(-Integer.MAX_VALUE, InfiniteInteger.valueOf(-Integer.MAX_VALUE).intValue());
    	infiniteInteger = InfiniteInteger.valueOf(Integer.MAX_VALUE).add(Integer.MAX_VALUE).add(1).negate();
    	assertEquals(-Integer.MAX_VALUE, infiniteInteger.intValue());
    }

    @Test
    public void littleEndian() {
    	assertSame(InfiniteInteger.ZERO, InfiniteInteger.littleEndian(Collections.emptyIterator(), true));
    	Iterator<Long> input = Arrays.asList(0L, 0L, 0L, 0L, 0L).iterator();
    	assertSame(InfiniteInteger.ZERO, InfiniteInteger.littleEndian(input, true));
    	input = Arrays.asList(1L, 1L, Long.MAX_VALUE, 0L).iterator();
    	assertEqualNodes(InfiniteInteger.littleEndian(input, true), -1, 1, 0, 1, 0, -1, Integer.MAX_VALUE);
    }

    @Test
    public void longValue() {
    	assertEquals(5, InfiniteInteger.valueOf(5).longValue());
    	assertEquals(Long.MAX_VALUE, InfiniteInteger.valueOf(Long.MAX_VALUE).longValue());
    	infiniteInteger = InfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(1);
    	assertEquals(Long.MAX_VALUE, infiniteInteger.longValue());

    	assertEquals(-1, InfiniteInteger.valueOf(-1).longValue());
    	assertEquals(-Long.MAX_VALUE, InfiniteInteger.valueOf(-Long.MAX_VALUE).longValue());
    	infiniteInteger = InfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(1).negate();
    	assertEquals(-Long.MAX_VALUE, infiniteInteger.longValue());
    }

    @Test
    public void longValueExact() {
    	infiniteInteger = InfiniteInteger.valueOf(Long.MAX_VALUE);
    	assertEquals(Long.MAX_VALUE, infiniteInteger.longValueExact());  //let throw on test fail

    	infiniteInteger = infiniteInteger.add(2);
    	try{infiniteInteger.longValueExact(); Assert.fail("Did not throw when > signed long.");}
    	catch(ArithmeticException e){}

    	try{infiniteInteger.add(Long.MAX_VALUE).longValueExact(); Assert.fail("Did not throw when > unsigned long.");}
    	catch(ArithmeticException e){}
    }

    @Test
    public void multiply_long() {
    	//simple case
    	assertEqualNodes(InfiniteInteger.valueOf(5).multiply(5), 1, 25);

    	//more than max int
    	assertEqualNodes(InfiniteInteger.valueOf(4_294_967_295L).multiply(-2), -1, (int) 4_294_967_294L, 1);

    	//more than max long
    	assertEqualNodes(InfiniteInteger.valueOf(Long.MAX_VALUE).multiply(2).add(2), 1, 0, 0, 1);

    	//multi digit
    	assertEqualNodes(InfiniteInteger.valueOf(-Long.MAX_VALUE).multiply(-Long.MAX_VALUE), 1, 1, 0, -1, 0x3FFF_FFFF);
    	//first pass should be: + 1, 7FFFFFFF, 7FFFFFFF
    	//second pass should be: + 0, 80000001, 7FFFFFFF, 3FFFFFFF
    }

    @Test
    @Ignore
    public void multiply_InfiniteInteger() {
    	//TODO: why is multiply_InfiniteInteger freezing?
    	//simple case
    	assertEqualNodes(InfiniteInteger.valueOf(5).multiply(InfiniteInteger.valueOf(5)), 1, 25);

    	//more than max int
    	assertEqualNodes(InfiniteInteger.valueOf(4_294_967_295L).multiply(InfiniteInteger.valueOf(-2)), -1, (int) 4_294_967_294L, 1);

    	//more than max long
    	assertEqualNodes(InfiniteInteger.valueOf(Long.MAX_VALUE).multiply(2).add(InfiniteInteger.valueOf(2)), 1, 0, 0, 1);

    	//multi digit
    	assertEqualNodes(InfiniteInteger.valueOf(-Long.MAX_VALUE).multiply(InfiniteInteger.valueOf(-Long.MAX_VALUE)), 1, 1, 0, -1, 0x3FFF_FFFF);
    	//first pass should be: + 1, 7FFFFFFF, 7FFFFFFF
    	//second pass should be: + 0, 80000001, 7FFFFFFF, 3FFFFFFF
    }

    @Test
	public void multiplyByPowerOf2() {
    	//simple case
    	assertEqualNodes(InfiniteInteger.valueOf(1).multiplyByPowerOf2(3), 1, 8);

    	//shift by x32
    	assertEqualNodes(InfiniteInteger.valueOf(1).multiplyByPowerOf2(64), 1, 0, 0, 1);

    	//shift more than 32
    	assertEqualNodes(InfiniteInteger.valueOf(1).multiplyByPowerOf2(35), 1, 0, 8);

    	//multiple starting nodes (shift not x32)
    	assertEqualNodes(InfiniteInteger.valueOf(Long.MAX_VALUE).multiplyByPowerOf2(34), 1, 0, -4, -1, 1);
    }

    @Test
	public void streamAllIntegers() {
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
	public void streamFibonacciSequence() {
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
	public void subtract_long() {
		//simple case
    	assertEqualNodes(InfiniteInteger.valueOf(10).subtract(5), 1, 5);

		//simple negative case
    	assertEqualNodes(InfiniteInteger.valueOf(5).subtract(10), -1, 5);

		//more than max int
    	assertEqualNodes(InfiniteInteger.valueOf(4_294_967_295L).subtract(1), 1, (int) 4_294_967_294L);

		//more than max long
		infiniteInteger = InfiniteInteger.valueOf(1).subtract(Long.MAX_VALUE).subtract(Long.MAX_VALUE).subtract(3);
    	assertEqualNodes(infiniteInteger, -1, 0, 0, 1);

		//borrow big
    	infiniteInteger = InfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2);
		infiniteInteger = infiniteInteger.subtract(1);
    	assertEqualNodes(infiniteInteger, 1, -1, -1);

    	//special case is negative but can't use Math.abs
    	assertEqualNodes(InfiniteInteger.valueOf(1).subtract(Long.MIN_VALUE), 1, 1, Integer.MIN_VALUE);
	}

    @Test
    public void subtract_InfiniteInteger() {
		//simple case
    	assertEqualNodes(InfiniteInteger.valueOf(10).subtract(InfiniteInteger.valueOf(5)), 1, 5);

		//more than max int
    	assertEqualNodes(InfiniteInteger.valueOf(4_294_967_295L).subtract(InfiniteInteger.valueOf(1)), 1, (int) 4_294_967_294L);

		//borrow big
    	infiniteInteger = InfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2);
		infiniteInteger = infiniteInteger.subtract(InfiniteInteger.valueOf(1));
    	assertEqualNodes(infiniteInteger, 1, -1, -1);
    }

	@Test
    public void valueOf_BigInteger() {
    	assertEquals(InfiniteInteger.valueOf(5), InfiniteInteger.valueOf(BigInteger.valueOf(5)));
    	assertEquals(InfiniteInteger.valueOf(Long.MAX_VALUE -5), InfiniteInteger.valueOf(BigInteger.valueOf(Long.MAX_VALUE -5)));

    	infiniteInteger = InfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2).negate();
    	BigInteger input = BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.valueOf(Long.MAX_VALUE)).add(BigInteger.valueOf(2)).negate();
    	assertEquals(infiniteInteger, InfiniteInteger.valueOf(input));
    }

    @Test
    public void valueOf_long() {
    	//simple case
    	assertEqualNodes(InfiniteInteger.valueOf(-5), -1, 5);

    	//more than max int
    	assertEqualNodes(InfiniteInteger.valueOf(8_589_934_592L), 1, 0, 2);

    	//prove that each node is unsigned
    	assertEqualNodes(InfiniteInteger.valueOf(2_147_483_648L), 1, (int) 2_147_483_648L);

    	//special case: can't use Math.abs
    	assertEqualNodes(InfiniteInteger.valueOf(Long.MIN_VALUE), -1, 0, Integer.MIN_VALUE);
    }

	private void assertEqualNodes(InfiniteInteger infiniteIntegerParam, int expectedSignum, int... expectedNodes) {
		assertEquals(generateInfiniteIntegerString(expectedSignum, expectedNodes), infiniteIntegerParam.toString());
	}

	//tightly coupled with the current debugging InfiniteInteger.toString()
	private String generateInfiniteIntegerString(int signum, int... nodeValues) {
		if(signum == 0) return "0";
		String returnValue = "+ ";
		if(signum == -1) returnValue = "- ";
		StringBuilder stringBuilder = new StringBuilder(returnValue);
		for (int node : nodeValues)
		{
			stringBuilder.append(Integer.toHexString(node).toUpperCase());
			stringBuilder.append(", ");
		}
		return stringBuilder.toString();
	}

}
