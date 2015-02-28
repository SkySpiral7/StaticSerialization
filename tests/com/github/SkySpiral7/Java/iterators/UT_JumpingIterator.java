package com.github.SkySpiral7.Java.iterators;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.ListIterator;

import org.junit.Before;
import org.junit.Test;

import com.github.SkySpiral7.Java.dataStructures.LinkedList;

/**
 * Don't bother testing the interface default since they all delegate to the static methods.
 */
public class UT_JumpingIterator {
	private LinkedList<Integer> linkedList;

	@Before
	public void setUp() {
		linkedList = new LinkedList<>(Arrays.asList(0, 1, 2, 3, 4, 5, 6));
	}

	@Test
    public void jumpToBeginning_Sequential() {
		ListIterator<Integer> iterator = linkedList.listIterator();
		iterator.next();
		iterator.next();
		iterator.next();
		JumpingIterator.jumpToBeginning(iterator);

    	assertEquals(0, iterator.next().intValue());
    }

	@Test
    public void jumpToBeginning_RandomAccess() {
		JumpingIterator<Integer> iterator = new JumpingIteratorExternalRandomAccess<>(linkedList);
		iterator.next();
		iterator.next();
		iterator.next();
		iterator.jumpToBeginning();

    	assertEquals(0, iterator.next().intValue());
    }

	@Test
    public void jumpToEnd_Sequential() {
		ListIterator<Integer> iterator = linkedList.listIterator();
		iterator.next();
		JumpingIterator.jumpToEnd(iterator);

    	assertEquals(6, iterator.previous().intValue());
    }

	@Test
    public void jumpToEnd_RandomAccess() {
		JumpingIterator<Integer> iterator = new JumpingIteratorExternalRandomAccess<>(linkedList);
		iterator.next();
		iterator.jumpToEnd();

    	assertEquals(6, iterator.previous().intValue());
    }

	@Test
    public void jumpByIndex_Sequential() {
		ListIterator<Integer> iterator = linkedList.listIterator();
		iterator.next();

		JumpingIterator.jumpByIndex(iterator, 2);
    	assertEquals(3, iterator.next().intValue());

		JumpingIterator.jumpByIndex(iterator, -1);
    	assertEquals(3, iterator.next().intValue());

		JumpingIterator.jumpByIndex(iterator, 200);
    	assertEquals(6, iterator.previous().intValue());

		JumpingIterator.jumpByIndex(iterator, -10);
    	assertEquals(0, iterator.next().intValue());
    }

	@Test
    public void jumpByIndex_RandomAccess() {
		JumpingIterator<Integer> iterator = new JumpingIteratorExternalRandomAccess<>(linkedList);
		iterator.next();

		iterator.jumpByIndex(2);
    	assertEquals(3, iterator.next().intValue());

    	iterator.jumpByIndex(-1);
    	assertEquals(3, iterator.next().intValue());

    	iterator.jumpByIndex(200);
    	assertEquals(6, iterator.previous().intValue());

    	iterator.jumpByIndex(-10);
    	assertEquals(0, iterator.next().intValue());
    }

	@Test
    public void jumpToIndex_Sequential() {
		ListIterator<Integer> iterator = linkedList.listIterator();
		iterator.next();

		JumpingIterator.jumpToIndex(iterator, 5);
    	assertEquals(5, iterator.nextIndex());

		JumpingIterator.jumpToIndex(iterator, -5);
    	assertEquals(0, iterator.nextIndex());

		JumpingIterator.jumpToIndex(iterator, 100);
    	assertEquals(7, iterator.nextIndex());

		JumpingIterator.jumpToIndex(iterator, 2);
    	assertEquals(2, iterator.nextIndex());
    }

	@Test
    public void jumpToIndex_RandomAccess() {
		JumpingIterator<Integer> iterator = new JumpingIteratorExternalRandomAccess<>(linkedList);
		iterator.next();

		iterator.jumpToIndex(5);
    	assertEquals(5, iterator.nextIndex());

    	iterator.jumpToIndex(-5);
    	assertEquals(0, iterator.nextIndex());

    	iterator.jumpToIndex(100);
    	assertEquals(7, iterator.nextIndex());

    	iterator.jumpToIndex(2);
    	assertEquals(2, iterator.nextIndex());
    }

}
