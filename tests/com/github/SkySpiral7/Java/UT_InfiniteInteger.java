package com.github.SkySpiral7.Java;

import static com.github.SkySpiral7.Java.pojo.Comparison.EQUAL_TO;
import static com.github.SkySpiral7.Java.util.ComparableSugar.is;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.lessThan;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.util.ListIterator;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

public class UT_InfiniteInteger {
    private InfiniteInteger infiniteInteger;

    @Test
    public void add_long() {
    	//simple case
    	infiniteInteger = InfiniteInteger.valueOf(5).add(5);
    	assertEquals(1, infiniteInteger.signum());
    	assertEquals(10, infiniteInteger.magnitudeHead.getData().intValue());
    	assertNull(infiniteInteger.magnitudeHead.getNext());

    	//more than max int
    	infiniteInteger = InfiniteInteger.valueOf(8_589_934_592L).add(5);
    	assertEquals(1, infiniteInteger.signum());
    	assertEquals(5, infiniteInteger.magnitudeHead.getData().intValue());
    	assertEquals(2, infiniteInteger.magnitudeHead.getNext().getData().intValue());
    	assertNull(infiniteInteger.magnitudeHead.getNext().getNext());

    	//more than max long
    	infiniteInteger = InfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2);
    	ListIterator<Integer> magnitudeIterator = infiniteInteger.magnitudeIterator();
    	assertEquals(1, infiniteInteger.signum());
    	assertEquals(0, magnitudeIterator.next().intValue());
    	assertEquals(0, magnitudeIterator.next().intValue());
    	assertEquals(1, magnitudeIterator.next().intValue());
    	assertFalse(magnitudeIterator.hasNext());

    	//special case is negative but can't use Math.abs
//    	infiniteInteger = InfiniteInteger.valueOf(1).add(Long.MIN_VALUE);
//    	assertEquals(-1, infiniteInteger.signum());
//    	assertEquals(0, infiniteInteger.magnitudeHead.getData().intValue());
//    	assertEquals(Integer.MIN_VALUE, infiniteInteger.magnitudeHead.getNext().getData().intValue());
//    	assertNull(infiniteInteger.magnitudeHead.getNext().getNext());
    	//TODO: this test requires subtraction
    }

    @Test
    public void add_InfiniteInteger() {
    	//simple case
    	infiniteInteger = InfiniteInteger.valueOf(5).add(InfiniteInteger.valueOf(5));
    	assertEquals(1, infiniteInteger.signum());
    	assertEquals(10, infiniteInteger.magnitudeHead.getData().intValue());
    	assertNull(infiniteInteger.magnitudeHead.getNext());

    	//more than max int
    	infiniteInteger = InfiniteInteger.valueOf(8_589_934_592L).add(InfiniteInteger.valueOf(5));
    	assertEquals(1, infiniteInteger.signum());
    	assertEquals(5, infiniteInteger.magnitudeHead.getData().intValue());
    	assertEquals(2, infiniteInteger.magnitudeHead.getNext().getData().intValue());
    	assertNull(infiniteInteger.magnitudeHead.getNext().getNext());

    	//more than max long
    	infiniteInteger = InfiniteInteger.valueOf(Long.MAX_VALUE).add(InfiniteInteger.valueOf(Long.MAX_VALUE)).add(InfiniteInteger.valueOf(2));
    	ListIterator<Integer> magnitudeIterator = infiniteInteger.magnitudeIterator();
    	assertEquals(1, infiniteInteger.signum());
    	assertEquals(0, magnitudeIterator.next().intValue());
    	assertEquals(0, magnitudeIterator.next().intValue());
    	assertEquals(1, magnitudeIterator.next().intValue());
    	assertFalse(magnitudeIterator.hasNext());
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
    	//TODO: more fast paths?
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
    @Ignore
    public void multiply_long() {
    	//simple case
    	infiniteInteger = InfiniteInteger.valueOf(5).multiply(5);
    	assertEquals(1, infiniteInteger.signum());
    	assertEquals(25, infiniteInteger.magnitudeHead.getData().intValue());
    	assertNull(infiniteInteger.magnitudeHead.getNext());

    	//more than max int
    	infiniteInteger = InfiniteInteger.valueOf(4_294_967_295L).multiply(-2);
    	assertEquals(-1, infiniteInteger.signum());
    	assertEquals(4_294_967_294L, Integer.toUnsignedLong(infiniteInteger.magnitudeHead.getData().intValue()));
    	assertEquals(1, infiniteInteger.magnitudeHead.getNext().getData().intValue());
    	assertNull(infiniteInteger.magnitudeHead.getNext().getNext());

    	//more than max long
    	infiniteInteger = InfiniteInteger.valueOf(Long.MAX_VALUE).multiply(2).add(2);
    	ListIterator<Integer> magnitudeIterator = infiniteInteger.magnitudeIterator();
    	assertEquals(1, infiniteInteger.signum());
    	assertEquals(0, magnitudeIterator.next().intValue());
    	assertEquals(0, magnitudeIterator.next().intValue());
    	assertEquals(1, magnitudeIterator.next().intValue());
    	assertFalse(magnitudeIterator.hasNext());

    	//multi digit
    	infiniteInteger = InfiniteInteger.valueOf(-Long.MAX_VALUE).multiply(-Long.MAX_VALUE);
    	magnitudeIterator = infiniteInteger.magnitudeIterator();
    	assertEquals(1, infiniteInteger.signum());
    	assertEquals(1, magnitudeIterator.next().intValue());
    	assertEquals(0, magnitudeIterator.next().intValue());
    	assertEquals(0xFFFF_FFFF, magnitudeIterator.next().intValue());
    	assertEquals(0x3FFF_FFFF, magnitudeIterator.next().intValue());
    	assertFalse(magnitudeIterator.hasNext());
    	//not sure about this math
    }

	//TODO: makes tests: shift left and right

    @Test
	public void subtract_long() {
		//simple case
		infiniteInteger = InfiniteInteger.valueOf(10).subtract(5);
		assertEquals(1, infiniteInteger.signum());
		assertEquals(5, infiniteInteger.magnitudeHead.getData().intValue());
		assertNull(infiniteInteger.magnitudeHead.getNext());

		//simple negative case
//		infiniteInteger = InfiniteInteger.valueOf(5).subtract(10);
//		assertEquals(-1, infiniteInteger.signum());
//		assertEquals(5, infiniteInteger.magnitudeHead.getData().intValue());
//		assertNull(infiniteInteger.magnitudeHead.getNext());

		//more than max int
		infiniteInteger = InfiniteInteger.valueOf(4_294_967_295L).subtract(1);
		assertEquals(1, infiniteInteger.signum());
		assertEquals(4_294_967_294L, Integer.toUnsignedLong(infiniteInteger.magnitudeHead.getData().intValue()));
		assertNull(infiniteInteger.magnitudeHead.getNext());

		//more than max long
//		infiniteInteger = InfiniteInteger.valueOf(1).subtract(Long.MAX_VALUE).subtract(Long.MAX_VALUE).subtract(3);
//		ListIterator<Integer> magnitudeIterator = infiniteInteger.magnitudeIterator();
//		assertEquals(-1, infiniteInteger.signum());
//		assertEquals(0, magnitudeIterator.next().intValue());
//		assertEquals(0, magnitudeIterator.next().intValue());
//		assertEquals(1, magnitudeIterator.next().intValue());
//		assertFalse(magnitudeIterator.hasNext());

		//borrow big
    	infiniteInteger = InfiniteInteger.valueOf(Long.MAX_VALUE).add(Long.MAX_VALUE).add(2);
		infiniteInteger = infiniteInteger.subtract(1);
		ListIterator<Integer> magnitudeIterator = infiniteInteger.magnitudeIterator();
		assertEquals(1, infiniteInteger.signum());
		assertEquals(-1, magnitudeIterator.next().intValue());
		assertEquals(-1, magnitudeIterator.next().intValue());
		assertFalse(magnitudeIterator.hasNext());
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
    	infiniteInteger = InfiniteInteger.valueOf(-5);
    	assertEquals(-1, infiniteInteger.signum());
    	assertEquals(5, infiniteInteger.magnitudeHead.getData().intValue());
    	assertNull(infiniteInteger.magnitudeHead.getNext());

    	//more than max int
    	infiniteInteger = InfiniteInteger.valueOf(8_589_934_592L);
    	assertEquals(1, infiniteInteger.signum());
    	assertEquals(0, infiniteInteger.magnitudeHead.getData().intValue());
    	assertEquals(2, infiniteInteger.magnitudeHead.getNext().getData().intValue());
    	assertNull(infiniteInteger.magnitudeHead.getNext().getNext());

    	//prove that each node is unsigned
    	infiniteInteger = InfiniteInteger.valueOf(2_147_483_648L);
    	assertEquals(1, infiniteInteger.signum());
    	assertEquals(2_147_483_648L, Integer.toUnsignedLong(infiniteInteger.magnitudeHead.getData().intValue()));
    	assertNull(infiniteInteger.magnitudeHead.getNext());

    	//special case: can't use Math.abs
    	infiniteInteger = InfiniteInteger.valueOf(Long.MIN_VALUE);
    	assertEquals(-1, infiniteInteger.signum());
    	assertEquals(0, infiniteInteger.magnitudeHead.getData().intValue());
    	assertEquals(Integer.MIN_VALUE, infiniteInteger.magnitudeHead.getNext().getData().intValue());
    	assertNull(infiniteInteger.magnitudeHead.getNext().getNext());
    }

}
