package com.github.SkySpiral7.Java.iterators;

import java.util.ListIterator;

/**
 * This is a decorator class for a ListIterator. The changed functionality is that
 * the data can't be changed (ie is read only). Add, set, and remove will throw if called
 * everything else delegates to the underlying ListIterator.
 *
 * @param <E> the element types contained in the list iterator
 * @see ReadOnlyIterator
 */
public class ReadOnlyListIterator<E> extends ReadOnlyIterator<E> implements ListIterator<E> {
	protected ListIterator<E> underlyingListIterator;  //exists so that I don't have to cast underlyingIterator everywhere

	public ReadOnlyListIterator(ListIterator<E> underlyingListIterator) {
		super(underlyingListIterator);  //sets super.underlyingIterator
		this.underlyingListIterator = underlyingListIterator;
	}

	@Override public boolean hasPrevious(){return underlyingListIterator.hasPrevious();}
	@Override public E previous(){return underlyingListIterator.previous();}
	@Override public int nextIndex(){return underlyingListIterator.nextIndex();}
	@Override public int previousIndex(){return underlyingListIterator.previousIndex();}

	@Override
	public final void set(E dummy) {
		throw new UnsupportedOperationException("This iterator is read only.");
	}

	@Override
	public final void add(E dummy) {
		throw new UnsupportedOperationException("This iterator is read only.");
	}

}
