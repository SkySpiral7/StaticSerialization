package src;

import java.util.Iterator;

public class ReadOnlyIterator<E> implements Iterator<E> {
	protected Iterator<E> underlyingIterator;

	public ReadOnlyIterator(Iterator<E> underlyingIterator) {
		this.underlyingIterator = underlyingIterator;
	}

	@Override public boolean hasNext(){return underlyingIterator.hasNext();}
	@Override public E next(){return underlyingIterator.next();}

	@Override
	public void remove() {
		throw new UnsupportedOperationException("This iterator is read only.");
	}

}
