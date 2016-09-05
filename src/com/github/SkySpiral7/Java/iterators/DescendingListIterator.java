package com.github.SkySpiral7.Java.iterators;

import java.util.ListIterator;

public class DescendingListIterator<E> implements ListIterator<E>
{
   protected final ListIterator<E> underlyingIterator;

   protected DescendingListIterator(ListIterator<E> iteratorToReverse) {this.underlyingIterator = iteratorToReverse;}

   public static <E> ListIterator<E> iterateBackwards(ListIterator<E> iteratorToReverse)
   {
      if (iteratorToReverse instanceof DescendingListIterator) return ((DescendingListIterator<E>) iteratorToReverse).underlyingIterator;
      return new DescendingListIterator<E>(iteratorToReverse);
   }

   public static <E> ListIterator<E> iterateBackwardsFromEnd(ListIterator<E> iteratorToReverse)
   {
      while (iteratorToReverse.hasNext()) { iteratorToReverse.next(); }
      return iterateBackwards(iteratorToReverse);
   }

   @Override
   public void remove() {underlyingIterator.remove();}

   @Override
   public void set(E newData) {underlyingIterator.set(newData);}

   @Override
   public void add(E newElement) {underlyingIterator.add(newElement);}

   @Override
   public boolean hasNext()
   {
      return underlyingIterator.hasPrevious();
   }

   @Override
   public E next()
   {
      return underlyingIterator.previous();
   }

   @Override
   public boolean hasPrevious()
   {
      return underlyingIterator.hasNext();
   }

   @Override
   public E previous()
   {
      return underlyingIterator.next();
   }

   @Override
   public int nextIndex()
   {
      return underlyingIterator.previousIndex();
   }

   @Override
   public int previousIndex()
   {
      return underlyingIterator.nextIndex();
   }

}
