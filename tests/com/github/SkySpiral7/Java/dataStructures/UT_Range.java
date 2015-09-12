package com.github.SkySpiral7.Java.dataStructures;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Test;

public class UT_Range
{
	@Test
   public void createArray() {
		final Range<Integer> range = new Range<Integer>(1, "..", 3);

		assertArrayEquals(new Integer[]{1, 2, 3}, range.createArray());
		assertArrayEquals(new Integer[]{1, 3}, range.createArray(2));

		assertArrayEquals(new int[]{1, 2, 3}, range.createArray(int[].class));
		assertArrayEquals(new byte[]{1, 2, 3}, range.createArray(byte[].class));
		assertArrayEquals(new long[]{1, 3}, range.createArray(long[].class, 2));

		assertArrayEquals(new Double[]{1d, 1.5d, 2d, 2.5d, 3d}, new Range<Double>(1d, "..", 3d).createArray(Double[].class, 0.5d));
	}

	@Test
   public void createList() {
		final Range<Integer> range = new Range<Integer>(1, "..", 3);

		assertEquals(Arrays.asList(new Integer[]{1, 2, 3}), range.createList());
		assertEquals(Arrays.asList(new Integer[]{1, 3}), range.createList(2));

		assertEquals(Arrays.asList(new Integer[]{1, 2, 3}), range.createList(Integer.class));
		assertEquals(Arrays.asList(new Byte[]{1, 2, 3}), range.createList(Byte.class));
		assertEquals(Arrays.asList(new Long[]{1L, 3L}), range.createList(Long.class, 2));
	}

	@Test
	public void contains() {
		final Range<Integer> range = new Range<Integer>(Range.inclusive(0), Range.exclusive(10));

		assertTrue(range.contains(0));
		assertTrue(range.contains(1));
		assertFalse(range.contains(-1));
		assertFalse(range.contains(10));
		assertFalse(range.contains(11));
	}

	@Test
	public void test_toString() {
		assertEquals("0 ..> 10", new Range<Integer>(Range.inclusive(0), Range.exclusive(10)).toString());
		assertEquals("0 <.. 10", new Range<Integer>(Range.exclusive(0), Range.inclusive(10)).toString());
		assertEquals("-5 .. 5", new Range<Integer>(Range.inclusive(-5), Range.inclusive(5)).toString());
	}

}
