package com.github.SkySpiral7.Java.iterators;

import java.util.ListIterator;

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
	public void set(E dummy) {
		throw new UnsupportedOperationException("This iterator is read only.");
	}

	@Override
	public void add(E dummy) {
		throw new UnsupportedOperationException("This iterator is read only.");
	}

}
