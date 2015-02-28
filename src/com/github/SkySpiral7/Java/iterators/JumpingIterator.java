package com.github.SkySpiral7.Java.iterators;

import java.util.Iterator;
import java.util.ListIterator;

/**
 * This list iterator can move more than one step at a time.
 * Such as causing it to jump between index 1 and the end.
 * These methods might not have meaning to infinite iterators.
 *
 * @param <E> the element type of the ListIterator
 */
public interface JumpingIterator<E> extends ListIterator<E> {
	/**
	 * Delegates to the static method.
	 * @see #jumpToBeginning(ListIterator)
	 */
	default public void jumpToBeginning(){jumpToBeginning(this);}
	/**
	 * After calling this method the iterator will be at the beginning of the list such that hasPrevious() is false.
	 * Note that if the iterator has no beginning then this method will be stuck in a loop forever.
	 *
	 * @param iteratorToUse the iterator changed
	 * @see ListIterator#hasPrevious()
	 */
	public static <T> void jumpToBeginning(ListIterator<T> iteratorToUse){while(iteratorToUse.hasPrevious()) iteratorToUse.previous();}

	/**
	 * Delegates to the static method.
	 * @see #jumpByIndex(ListIterator, int)
	 */
	default public void jumpByIndex(int jumpAmount){jumpByIndex(this, jumpAmount);}
	/**
	 * <p>This method will move the iterator forward or backward based on the jumpAmount. If the jumpAmount is positive
	 * the iterator will be moved forward that number of times, if the jumpAmount is negative the iterator will be moved
	 * backward that number of times (passing in 0 does nothing). For example passing in 3 is the same as calling next()
	 * 3 times and passing in -2 is the same as calling previous() twice.</p>
	 *
	 * <p>Note that unlike calling next() or previous() multiple times this method has an additional feature of
	 * not throwing NoSuchElementException. If the iterator only has 3 items remaining calling jumpByIndex(100)
	 * will instead put the iterator at the end, likewise if there is only 1 previous element passing in -2 will instead
	 * go to the beginning.</p>
	 *
	 * @param iteratorToUse the iterator changed
	 * @param jumpAmount
	 * @see ListIterator#next()
	 * @see ListIterator#previous()
	 * @see #jumpToIndex(ListIterator, int)
	 */
	public static <T> void jumpByIndex(ListIterator<T> iteratorToUse, int jumpAmount)
	{
		while(jumpAmount > 0 && iteratorToUse.hasNext()){iteratorToUse.next(); jumpAmount--;}
		while(jumpAmount < 0 && iteratorToUse.hasPrevious()){iteratorToUse.previous(); jumpAmount++;}
	}
	/**
	 * Delegates to the static method.
	 * @see #jumpByIndex(ListIterator, int)
	 */
	default public void jumpToIndex(int destination){jumpByIndex(this, destination);}
	/**
	 * <p>After calling this method the iterator will be in a position such that nextIndex() matches the destination.
	 * If the destination is not a valid index then the iterator will instead be at the beginning or end (depending
	 * on if destination was less than 0 or greater than size). Therefore passing in -10 will be treated as 0
	 * and will go to the beginning instead of throwing.</p>
	 *
	 * <p>Note that this method assumes that the iterator given has indexes that are as expected (with the first index
	 * being 0 and the last being size). This is a necessary assumption because otherwise there would be no way of knowing
	 * whether next() or previous() should be called to reach the destination index.</p>
	 *
	 * @param iteratorToUse the iterator changed
	 * @param destination desired index
	 * @see ListIterator#nextIndex()
	 * @see #jumpByIndex(ListIterator, int)
	 */
	public static <T> void jumpToIndex(ListIterator<T> iteratorToUse, int destination)
	{
		jumpByIndex(iteratorToUse, (destination - iteratorToUse.nextIndex()));
	}

	/**
	 * Delegates to the static method.
	 * @see #jumpToEnd(ListIterator)
	 */
	default public void jumpToEnd(){jumpToEnd(this);}
	/**
	 * After calling this method the iterator will be at the end of the list such that hasNext() is false.
	 * Note that if the iterator has no end then this method will be stuck in a loop forever.
	 *
	 * @param iteratorToUse the iterator changed
	 * @see ListIterator#hasNext()
	 */
	public static <T> void jumpToEnd(Iterator<T> iteratorToUse){while(iteratorToUse.hasNext()) iteratorToUse.next();}
}
