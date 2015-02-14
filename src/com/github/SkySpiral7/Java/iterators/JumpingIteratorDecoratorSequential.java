package com.github.SkySpiral7.Java.iterators;

import java.util.ListIterator;

public class JumpingIteratorDecoratorSequential<E> implements JumpingIterator<E> {
	protected ListIterator<E> underlyingListIterator;

	public JumpingIteratorDecoratorSequential(ListIterator<E> underlyingListIterator){this.underlyingListIterator = underlyingListIterator;}

	@Override public boolean hasNext(){return underlyingListIterator.hasNext();}
	@Override public E next(){return underlyingListIterator.next();}
	@Override public boolean hasPrevious(){return underlyingListIterator.hasPrevious();}
	@Override public E previous(){return underlyingListIterator.previous();}
	@Override public int nextIndex(){return underlyingListIterator.nextIndex();}
	@Override public int previousIndex(){return underlyingListIterator.previousIndex();}
	@Override public void remove(){underlyingListIterator.remove();}
	@Override public void set(E newData){underlyingListIterator.set(newData);}
	@Override public void add(E newElement){underlyingListIterator.add(newElement);}

	//don't need to implement any methods for JumpingIterator because they all have defaults
}
