package com.github.skySpiral7.java.staticSerialization.testClasses;

import java.util.AbstractSet;
import java.util.Arrays;
import java.util.Collection;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Set;

/**
 * A very simple set wrapper for IdentityHashMap.
 * The set uses pointer equality instead of .equals to determine if an object is redundant.
 *
 * @param <E> the data type to be stored
 *
 * @see IdentityHashMap
 */
public class IdentityHashSet<E> extends AbstractSet<E> implements Set<E>
{
   private transient final IdentityHashMap<E, Boolean> dataMap;

   /**
    * The no-arg construct creates an empty set.
    */
   public IdentityHashSet()
   {
      super();
      dataMap = new IdentityHashMap<>();
   }

   public IdentityHashSet(final int expectedMaxSize)
   {
      super();
      dataMap = new IdentityHashMap<>(expectedMaxSize);
   }

   public IdentityHashSet(final Collection<? extends E> initialElements)
   {
      this(initialElements.size());
      this.addAll(initialElements);
   }

   public IdentityHashSet(final E[] initialElements){this(Arrays.asList(initialElements));}

   @Override
   public int size()
   {
      return dataMap.size();
   }

   @Override
   public boolean contains(final Object elementToFind)
   {
      return dataMap.containsKey(elementToFind);
   }

   @Override
   public Iterator<E> iterator()
   {
      return dataMap.keySet().iterator();
   }

   @Override
   public Object[] toArray()
   {
      return dataMap.keySet().toArray();
   }

   @Override
   public <T> T[] toArray(final T[] destinationArray)
   {
      return dataMap.keySet().toArray(destinationArray);
   }

   @Override
   public boolean add(final E newElement)
   {
      if (dataMap.containsKey(newElement)) return false;
      dataMap.put(newElement, Boolean.TRUE);  //always returns null
      return true;
   }

   @Override
   public boolean remove(final Object elementToRemove)
   {
      if (dataMap.containsKey(elementToRemove)) return false;
      return dataMap.remove(elementToRemove);  //previous value is always true
   }

   @Override
   public void clear()
   {
      dataMap.clear();
   }

}
