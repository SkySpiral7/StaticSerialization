package com.github.SkySpiral7.Java;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import java.math.BigInteger;
import java.util.ListIterator;

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
    public void fastPaths() {
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
    	assertNull(infiniteInteger.magnitudeHead.getNext());
    	assertEquals(2_147_483_648L, Integer.toUnsignedLong(infiniteInteger.magnitudeHead.getData().intValue()));
    }

}
