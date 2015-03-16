package com.github.SkySpiral7.Java.numbers;

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
import java.util.ListIterator;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import com.github.SkySpiral7.Java.iterators.JumpingIterator;
import com.github.SkySpiral7.Java.pojo.IntegerQuotient;

/**
 * Methods that simply delegate do not need a test.
 * Additionally the following do not need a test:
 * the other versions of littleEndian and bigEndian, magnitude Iterator and Stream,
 * getMagnitudeTail, selfPower, factorial, abs, negate, signum, isNaN, isInfinite, isFinite, signalNaN,
 * the other versions of equals, hashCode, copy, toFile (but toString should be tested when finished)
 */
public class UT_MutableInfiniteInteger {
    private MutableInfiniteInteger infiniteInteger;
	//TODO: make tests to ensure that this is mutated but not param

    @Test
    public void add_long() {
    	//simple case
    	assertEqualNodes(MutableInfiniteInteger.valueOf(5).add(5), 1, 10);

    	//simple negative case
    	assertEqualNodes(MutableInfiniteInteger.valueOf(-5).add(-5), -1, 10);

    	//more than max int
    	assertEqualNodes(MutableInfiniteInteger.valueOf(8_589_934_592L).add(5), 1, 5, 2);

    	//more than max long
    	assertEqualNodes(MutableInfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2), 1, 0, 0, 1);

    	//special case is negative but can't use Math.abs
    	assertEqualNodes(MutableInfiniteInteger.valueOf(-1).add(Long.MIN_VALUE), -1, 1, Integer.MIN_VALUE);
    }

    @Test
    public void add_InfiniteInteger() {
    	//simple case
    	assertEqualNodes(MutableInfiniteInteger.valueOf(5).add(MutableInfiniteInteger.valueOf(5)), 1, 10);

    	//simple negative case
    	assertEqualNodes(MutableInfiniteInteger.valueOf(-5).add(MutableInfiniteInteger.valueOf(-5)), -1, 10);

    	//more than max int
    	assertEqualNodes(MutableInfiniteInteger.valueOf(8_589_934_592L).add(MutableInfiniteInteger.valueOf(5)), 1, 5, 2);

    	//more than max long
    	infiniteInteger = MutableInfiniteInteger.valueOf(Long.MAX_VALUE).add(MutableInfiniteInteger.valueOf(Long.MAX_VALUE)).add(MutableInfiniteInteger.valueOf(2));
    	assertEqualNodes(infiniteInteger, 1, 0, 0, 1);
    }

    @Test
    @Ignore  //ignored because the code doesn't exist yet and is WAY too slow
    public void bigIntegerValue() {
    	final BigInteger bigIntMaxValueBig = MutableInfiniteInteger.calculateMaxBigInteger();
    	final MutableInfiniteInteger bigIntMaxValueInf = MutableInfiniteInteger.calculateMaxBigIntegerAsInfiniteInteger();
    	final BigInteger negativeBigIntMaxValueBig = bigIntMaxValueBig.negate();
    	final MutableInfiniteInteger negativeBigIntMaxValueInf = bigIntMaxValueInf.negate();

    	assertEquals(BigInteger.valueOf(5), MutableInfiniteInteger.valueOf(5).bigIntegerValue());
    	assertEquals(bigIntMaxValueBig, bigIntMaxValueInf.bigIntegerValue());
    	infiniteInteger = bigIntMaxValueInf.multiplyByPowerOf2(2).add(3);
    	assertEquals(bigIntMaxValueBig, infiniteInteger.bigIntegerValue());

    	assertEquals(BigInteger.valueOf(-1), MutableInfiniteInteger.valueOf(-1).bigIntegerValue());
    	assertEquals(negativeBigIntMaxValueBig, negativeBigIntMaxValueInf.bigIntegerValue());
    	infiniteInteger = negativeBigIntMaxValueInf.multiplyByPowerOf2(2).subtract(3);
    	assertEquals(negativeBigIntMaxValueBig, infiniteInteger.bigIntegerValue());
    }

    @Test
    @Ignore  //ignored because WAY too slow
    public void bigIntegerValueExact() {
    	infiniteInteger = MutableInfiniteInteger.calculateMaxBigIntegerAsInfiniteInteger();
    	assertEquals(MutableInfiniteInteger.calculateMaxBigInteger(), infiniteInteger.bigIntegerValueExact());  //let throw on test fail

    	try{infiniteInteger.add(1).bigIntegerValueExact(); Assert.fail("Did not throw when > BigInteger.");}
    	catch(ArithmeticException e){}
    }

    @Test
    public void compareTo() {
    	//don't use hamcrest for these because they would use .equals
    	infiniteInteger = MutableInfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2);
    	assertTrue(is(infiniteInteger, EQUAL_TO, infiniteInteger));  //same object
    	MutableInfiniteInteger mutableInfiniteInteger = MutableInfiniteInteger.valueOf(123);
    	assertTrue(is(mutableInfiniteInteger.copy(), EQUAL_TO, mutableInfiniteInteger));  //different object same value

    	//use hamcrest for rest to get a more meaningful failure message
    	assertThat(MutableInfiniteInteger.valueOf(-5), lessThan(MutableInfiniteInteger.valueOf(5)));
    	assertThat(MutableInfiniteInteger.valueOf(5), greaterThan(MutableInfiniteInteger.valueOf(-5)));
    	assertThat(MutableInfiniteInteger.valueOf(10), greaterThan(MutableInfiniteInteger.valueOf(5)));
    	assertThat(infiniteInteger, greaterThan(MutableInfiniteInteger.valueOf(10)));  //left has more nodes
    	assertThat(infiniteInteger.copy().add(1), greaterThan(infiniteInteger));  //same node count but different value
    	infiniteInteger = MutableInfiniteInteger.valueOf(Integer.MAX_VALUE).add(1);
    	assertThat(infiniteInteger.copy().add(1), greaterThan(infiniteInteger));  //make sure nodes are compared unsigned
    }

    @Test
    public void compareTo_special() {
    	//TODO: make test
    }

    @Test
	public void divide() {
    	//simple case
    	assertDivision(MutableInfiniteInteger.valueOf(10).divide(5), 1, new int[]{2}, new int[]{0});

    	//simple negative remainder
    	assertDivision(MutableInfiniteInteger.valueOf(-11).divide(5), -1, new int[]{2}, new int[]{1});

    	//this is too large for now
    	//assertDivision(MutableInfiniteInteger.valueOf(Long.MAX_VALUE).add(1).divide(2), 1, new int[]{0, Integer.MIN_VALUE}, new int[]{0});
    }

    @Test
	public void divideByPowerOf2DropRemainder() {
    	//simple case
    	assertEqualNodes(MutableInfiniteInteger.valueOf(1024).divideByPowerOf2DropRemainder(3), 1, 128);

    	//shift by x32
    	infiniteInteger = MutableInfiniteInteger.valueOf(1).multiplyByPowerOf2(64).add(Long.MAX_VALUE);
    	assertEqualNodes(infiniteInteger.divideByPowerOf2DropRemainder(64), 1, 1);

    	//shift more than 32
    	infiniteInteger = MutableInfiniteInteger.valueOf(1).multiplyByPowerOf2(32*3).subtract(1);  //3 nodes all high
    	assertEqualNodes(infiniteInteger.divideByPowerOf2DropRemainder(35), 1, -1, 0x1FFF_FFFF);
    }

    @Test
    public void equals() {
    	assertEquals(MutableInfiniteInteger.valueOf(10), MutableInfiniteInteger.valueOf(10));
    	assertEquals(MutableInfiniteInteger.valueOf(5).add(5), MutableInfiniteInteger.valueOf(7).add(3));
    	infiniteInteger = MutableInfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2);
    	assertEquals(infiniteInteger, infiniteInteger);
    	MutableInfiniteInteger mutableInfiniteInteger = MutableInfiniteInteger.valueOf(123);
    	assertEquals(mutableInfiniteInteger.copy(), mutableInfiniteInteger);
    	assertNotEquals(infiniteInteger.copy().add(1), infiniteInteger);
    }

    @Test
    public void fastPaths() {
    	//TODO: more fast paths but move them into each other test
    	assertSame(MutableInfiniteInteger.POSITIVE_INFINITITY, MutableInfiniteInteger.POSITIVE_INFINITITY.add(12));
    	assertSame(MutableInfiniteInteger.NEGATIVE_INFINITITY, MutableInfiniteInteger.NEGATIVE_INFINITITY.add(12));
    	assertSame(MutableInfiniteInteger.NaN, MutableInfiniteInteger.NaN.add(12));

    	assertSame(MutableInfiniteInteger.POSITIVE_INFINITITY, MutableInfiniteInteger.POSITIVE_INFINITITY.add(BigInteger.TEN));
    	assertSame(MutableInfiniteInteger.NEGATIVE_INFINITITY, MutableInfiniteInteger.NEGATIVE_INFINITITY.add(BigInteger.TEN));
    	assertSame(MutableInfiniteInteger.NaN, MutableInfiniteInteger.NaN.add(BigInteger.TEN));

    	MutableInfiniteInteger mutableInfiniteInteger = MutableInfiniteInteger.valueOf(12);
    	assertSame(mutableInfiniteInteger, mutableInfiniteInteger.add(0));
    	assertSame(mutableInfiniteInteger, mutableInfiniteInteger.add(MutableInfiniteInteger.valueOf(0)));

    	//must use debugger to see if the fast path was used for these
    	//these ones should not be moved since they are not visible
    	/*
    	MutableInfiniteInteger.valueOf(BigInteger.TEN);
    	/**/
    }

    @Test
    public void intValue() {
    	assertEquals(5, MutableInfiniteInteger.valueOf(5).intValue());
    	assertEquals(Integer.MAX_VALUE, MutableInfiniteInteger.valueOf(Integer.MAX_VALUE).intValue());
    	infiniteInteger = MutableInfiniteInteger.valueOf(Integer.MAX_VALUE).add(Integer.MAX_VALUE).add(1);
    	assertEquals(Integer.MAX_VALUE, infiniteInteger.intValue());

    	assertEquals(-1, MutableInfiniteInteger.valueOf(-1).intValue());
    	assertEquals(-Integer.MAX_VALUE, MutableInfiniteInteger.valueOf(-Integer.MAX_VALUE).intValue());
    	infiniteInteger = MutableInfiniteInteger.valueOf(Integer.MAX_VALUE).add(Integer.MAX_VALUE).add(1).negate();
    	assertEquals(-Integer.MAX_VALUE, infiniteInteger.intValue());
    }

    @Test
	public void iterateAllIntegers() {
		ListIterator<MutableInfiniteInteger> integerIterator = MutableInfiniteInteger.iterateAllIntegers();
		assertEquals(MutableInfiniteInteger.valueOf(0), integerIterator.previous());
		integerIterator.next();
		assertEquals(MutableInfiniteInteger.valueOf(1), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(2), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(3), integerIterator.next());
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
    public void littleEndian() {
    	assertEquals(MutableInfiniteInteger.valueOf(0), MutableInfiniteInteger.littleEndian(Collections.emptyIterator(), true));
    	Iterator<Long> input = Arrays.asList(0L, 0L, 0L, 0L, 0L).iterator();
    	assertEquals(MutableInfiniteInteger.valueOf(0), MutableInfiniteInteger.littleEndian(input, true));
    	input = Arrays.asList(1L, 1L, Long.MAX_VALUE, 0L).iterator();
    	assertEqualNodes(MutableInfiniteInteger.littleEndian(input, true), -1, 1, 0, 1, 0, -1, Integer.MAX_VALUE);
    }

    @Test
    public void longValue() {
    	assertEquals(5, MutableInfiniteInteger.valueOf(5).longValue());
    	assertEquals(Long.MAX_VALUE, MutableInfiniteInteger.valueOf(Long.MAX_VALUE).longValue());
    	infiniteInteger = MutableInfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(1);
    	assertEquals(Long.MAX_VALUE, infiniteInteger.longValue());

    	assertEquals(-1, MutableInfiniteInteger.valueOf(-1).longValue());
    	assertEquals(-Long.MAX_VALUE, MutableInfiniteInteger.valueOf(-Long.MAX_VALUE).longValue());
    	infiniteInteger = MutableInfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(1).negate();
    	assertEquals(-Long.MAX_VALUE, infiniteInteger.longValue());
    }

    @Test
    public void longValueExact() {
    	infiniteInteger = MutableInfiniteInteger.valueOf(Long.MAX_VALUE);
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
    	assertEqualNodes(MutableInfiniteInteger.valueOf(5).multiply(5), 1, 25);

    	//more than max int
    	assertEqualNodes(MutableInfiniteInteger.valueOf(4_294_967_295L).multiply(-2), -1, (int) 4_294_967_294L, 1);

    	//more than max long
    	assertEqualNodes(MutableInfiniteInteger.valueOf(Long.MAX_VALUE).multiply(2).add(2), 1, 0, 0, 1);

    	//multi digit
    	assertEqualNodes(MutableInfiniteInteger.valueOf(-Long.MAX_VALUE).multiply(-Long.MAX_VALUE), 1, 1, 0, -1, 0x3FFF_FFFF);
    	//first pass should be: + 1, 7FFFFFFF, 7FFFFFFF
    	//second pass should be: + 0, 80000001, 7FFFFFFF, 3FFFFFFF
    }

    @Test
    public void multiply_InfiniteInteger() {
    	//simple case
    	assertEqualNodes(MutableInfiniteInteger.valueOf(5).multiply(MutableInfiniteInteger.valueOf(5)), 1, 25);

    	//more than max int
    	assertEqualNodes(MutableInfiniteInteger.valueOf(4_294_967_295L).multiply(MutableInfiniteInteger.valueOf(-2)), -1, (int) 4_294_967_294L, 1);

    	//more than max long
    	assertEqualNodes(MutableInfiniteInteger.valueOf(Long.MAX_VALUE).multiply(2).add(MutableInfiniteInteger.valueOf(2)), 1, 0, 0, 1);

    	//multi digit
    	assertEqualNodes(MutableInfiniteInteger.valueOf(-Long.MAX_VALUE).multiply(MutableInfiniteInteger.valueOf(-Long.MAX_VALUE)), 1, 1, 0, -1, 0x3FFF_FFFF);
    	//first pass should be: + 1, 7FFFFFFF, 7FFFFFFF
    	//second pass should be: + 0, 80000001, 7FFFFFFF, 3FFFFFFF
    }

    @Test
	public void multiplyByPowerOf2() {
    	//simple case
    	assertEqualNodes(MutableInfiniteInteger.valueOf(1).multiplyByPowerOf2(3), 1, 8);

    	//shift by x32
    	assertEqualNodes(MutableInfiniteInteger.valueOf(1).multiplyByPowerOf2(64), 1, 0, 0, 1);

    	//shift more than 32
    	assertEqualNodes(MutableInfiniteInteger.valueOf(1).multiplyByPowerOf2(35), 1, 0, 8);

    	//multiple starting nodes (shift not x32)
    	assertEqualNodes(MutableInfiniteInteger.valueOf(Long.MAX_VALUE).multiplyByPowerOf2(34), 1, 0, -4, -1, 1);
    }

    @Test
	public void power() {
    	//simple case
    	assertEqualNodes(MutableInfiniteInteger.valueOf(3).power(3), 1, 27);

    	//multiple ending nodes
    	assertEqualNodes(MutableInfiniteInteger.valueOf(0x800).power(3), 1, 0, 2);

    	//multiple starting nodes
    	assertEqualNodes(MutableInfiniteInteger.valueOf(Long.MIN_VALUE).power(3), -1, 0, 0, 0, 0, 0, 0x2000_0000);
    }

    @Test
	public void streamAllIntegers() {
		Iterator<MutableInfiniteInteger> integerIterator = MutableInfiniteInteger.streamAllIntegers().iterator();
		assertEquals(MutableInfiniteInteger.valueOf(0), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(1), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(-1), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(2), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(-2), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(3), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(-3), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(4), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(-4), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(5), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(-5), integerIterator.next());
		assertTrue(integerIterator.hasNext());  //continues forever
	}

    @Test
	public void streamFibonacciSequence() {
		Iterator<MutableInfiniteInteger> integerIterator = MutableInfiniteInteger.streamFibonacciSequence().iterator();
		assertEquals(MutableInfiniteInteger.valueOf(0), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(1), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(1), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(2), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(3), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(5), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(8), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(13), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(21), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(34), integerIterator.next());
		assertEquals(MutableInfiniteInteger.valueOf(55), integerIterator.next());
		assertTrue(integerIterator.hasNext());  //continues forever
	}

	@Test
	public void subtract_long() {
		//simple case
    	assertEqualNodes(MutableInfiniteInteger.valueOf(10).subtract(5), 1, 5);

		//simple negative case
    	assertEqualNodes(MutableInfiniteInteger.valueOf(5).subtract(10), -1, 5);

		//more than max int
    	assertEqualNodes(MutableInfiniteInteger.valueOf(4_294_967_295L).subtract(1), 1, (int) 4_294_967_294L);

		//more than max long
		infiniteInteger = MutableInfiniteInteger.valueOf(1).subtract(Long.MAX_VALUE).subtract(Long.MAX_VALUE).subtract(3);
    	assertEqualNodes(infiniteInteger, -1, 0, 0, 1);

		//borrow big
    	infiniteInteger = MutableInfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2);
		infiniteInteger = infiniteInteger.subtract(1);
    	assertEqualNodes(infiniteInteger, 1, -1, -1);

    	//special case is negative but can't use Math.abs
    	assertEqualNodes(MutableInfiniteInteger.valueOf(1).subtract(Long.MIN_VALUE), 1, 1, Integer.MIN_VALUE);
	}

    @Test
    public void subtract_InfiniteInteger() {
		//simple case
    	assertEqualNodes(MutableInfiniteInteger.valueOf(10).subtract(MutableInfiniteInteger.valueOf(5)), 1, 5);

		//more than max int
    	assertEqualNodes(MutableInfiniteInteger.valueOf(4_294_967_295L).subtract(MutableInfiniteInteger.valueOf(1)), 1, (int) 4_294_967_294L);

		//borrow big
    	infiniteInteger = MutableInfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2);
		infiniteInteger = infiniteInteger.subtract(MutableInfiniteInteger.valueOf(1));
    	assertEqualNodes(infiniteInteger, 1, -1, -1);
    }

	@Test
    public void valueOf_BigInteger() {
    	assertEquals(MutableInfiniteInteger.valueOf(5), MutableInfiniteInteger.valueOf(BigInteger.valueOf(5)));
    	assertEquals(MutableInfiniteInteger.valueOf(Long.MAX_VALUE -5), MutableInfiniteInteger.valueOf(BigInteger.valueOf(Long.MAX_VALUE -5)));

    	infiniteInteger = MutableInfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2).negate();
    	BigInteger input = BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.valueOf(Long.MAX_VALUE)).add(BigInteger.valueOf(2)).negate();
    	assertEquals(infiniteInteger, MutableInfiniteInteger.valueOf(input));
    }

    @Test
    public void valueOf_long() {
    	//simple case
    	assertEqualNodes(MutableInfiniteInteger.valueOf(-5), -1, 5);

    	//more than max int
    	assertEqualNodes(MutableInfiniteInteger.valueOf(8_589_934_592L), 1, 0, 2);

    	//prove that each node is unsigned
    	assertEqualNodes(MutableInfiniteInteger.valueOf(2_147_483_648L), 1, (int) 2_147_483_648L);

    	//special case: can't use Math.abs
    	assertEqualNodes(MutableInfiniteInteger.valueOf(Long.MIN_VALUE), -1, 0, Integer.MIN_VALUE);
    }

	private void assertDivision(IntegerQuotient<MutableInfiniteInteger> divisionResult, int wholeSign, int[] wholeNodes, int[] remainderNodes) {
		assertEqualNodes(divisionResult.getWholeResult(), wholeSign, wholeNodes);
		if(remainderNodes.length == 1 && remainderNodes[0] == 0) assertEqualNodes(divisionResult.getRemainder(), 0, 0);
		else assertEqualNodes(divisionResult.getRemainder(), 1, remainderNodes);
	}

	private void assertEqualNodes(MutableInfiniteInteger infiniteIntegerParam, int expectedSignum, int... expectedNodes) {
		assertEquals(generateInfiniteIntegerString(expectedSignum, expectedNodes), infiniteIntegerParam.toString());
	}

	//tightly coupled with the current debugging MutableInfiniteInteger.toString()
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
