package com.github.SkySpiral7.Java.dataStructures;

import java.util.AbstractList;
import java.util.List;

/**
 * This interface was created so that any class may look at the modCount which is defined in AbstractList.
 *
 * @param <E>
 *       the data type of the list
 * @see AbstractList
 */
public interface ModCountList<E> extends List<E>
{
   public int getModCount();
}
