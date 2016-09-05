package com.github.SkySpiral7.Java.iterators;

import java.util.Iterator;

/**
 * This is a decorator class for an Iterator. The changed functionality is that
 * the data can't be changed (ie is read only). Remove will throw if called
 * everything else delegates to the underlying ListIterator.
 *
 * @param <E>
 *       the element types contained in the list iterator
 * @see ReadOnlyListIterator
 */
public class ReadOnlyIterator<E> implements Iterator<E>
{
   protected Iterator<E> underlyingIterator;

   public ReadOnlyIterator(Iterator<E> underlyingIterator)
   {
      this.underlyingIterator = underlyingIterator;
   }

   @Override
   public boolean hasNext() {return underlyingIterator.hasNext();}

   @Override
   public E next() {return underlyingIterator.next();}

   @Override
   public final void remove()
   {
      throw new UnsupportedOperationException("This iterator is read only.");
   }

}
