package com.github.SkySpiral7.Java.iterators;

import java.util.AbstractList;
import java.util.ConcurrentModificationException;
import java.util.ListIterator;
import java.util.NoSuchElementException;

import com.github.SkySpiral7.Java.dataStructures.ModCountList;

/**
 * Copied from AbstractList.ListItr with few changes. In addition to those defined IteratorExternal the underlyingList must also
 * have the following methods defined: set(int, E), add(int, E).
 *
 * @param <E> the data type of the list
 *
 * @see AbstractList
 */
public class ListIteratorExternal<E> extends IteratorExternal<E> implements ListIterator<E>
{

   public ListIteratorExternal(ModCountList<E> underlyingList, int initialIndex){super(underlyingList, initialIndex);}

   public ListIteratorExternal(ModCountList<E> underlyingList){super(underlyingList);}

   public boolean hasPrevious()
   {
      return cursor != 0;
   }

   public E previous()
   {
      checkForComodification();
      try
      {
         int i = cursor - 1;
         E previous = underlyingList.get(i);
         lastRet = cursor = i;
         return previous;
      }
      catch (IndexOutOfBoundsException e)
      {
         checkForComodification();
         throw new NoSuchElementException();
      }
   }

   public int nextIndex()
   {
      return cursor;
   }

   public int previousIndex()
   {
      return cursor - 1;
   }

   public void set(E e)
   {
      if (lastRet < 0) throw new IllegalStateException();
      checkForComodification();

      try
      {
         underlyingList.set(lastRet, e);
         expectedModCount = underlyingList.getModCount();
      }
      catch (IndexOutOfBoundsException ex)
      {
         throw new ConcurrentModificationException();
      }
   }

   public void add(E e)
   {
      checkForComodification();

      try
      {
         underlyingList.add(cursor, e);
         lastRet = -1;
         cursor++;
         expectedModCount = underlyingList.getModCount();
      }
      catch (IndexOutOfBoundsException ex)
      {
         throw new ConcurrentModificationException();
      }
   }
}
