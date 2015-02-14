package com.github.SkySpiral7.Java.iterators;

import java.util.ListIterator;

public class ReadOnlyListIterator<E> implements ListIterator<E> {
	//TODO: have ReadOnlyList extend the non-list
	protected ListIterator<E> underlyingIterator;

	public ReadOnlyListIterator(ListIterator<E> underlyingIterator) {
		this.underlyingIterator = underlyingIterator;
	}

	@Override public boolean hasNext(){return underlyingIterator.hasNext();}
	@Override public E next(){return underlyingIterator.next();}
	@Override public boolean hasPrevious(){return underlyingIterator.hasPrevious();}
	@Override public E previous(){return underlyingIterator.previous();}
	@Override public int nextIndex(){return underlyingIterator.nextIndex();}
	@Override public int previousIndex(){return underlyingIterator.previousIndex();}

	@Override
	public void remove() {
		throw new UnsupportedOperationException("This iterator is read only.");
	}

	@Override
	public void set(E dummy) {
		throw new UnsupportedOperationException("This iterator is read only.");
	}

	@Override
	public void add(E dummy) {
		throw new UnsupportedOperationException("This iterator is read only.");
	}

}
