package src.defaultImplementations;

import java.util.AbstractList;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.RandomAccess;

import src.ModCountList;

/**
 * Copied from AbstractList.Itr with few changes. Note that the underlyingList must have the following methods defined:
 * size(), get(int), remove(int), getModCount().
 * @param <E> the data type of the list
 * @see AbstractList
 */
public class IteratorExternal<E> implements Iterator<E>, RandomAccess {
    /**
     * Index of element to be returned by subsequent call to next.
     */
    protected int cursor;

    /**
     * Index of element returned by most recent call to next or
     * previous.  Reset to -1 if this element is deleted by a call
     * to remove.
     */
    protected int lastRet;

    /**
     * The modCount value that the iterator believes that the backing
     * List should have.  If this expectation is violated, the iterator
     * has detected concurrent modification.
     */
    protected int expectedModCount;

    protected ModCountList<E> underlyingList;

    public IteratorExternal(ModCountList<E> underlyingList, int initialIndex) {
    	cursor = initialIndex;
    	lastRet = -1;
    	this.underlyingList = underlyingList;
        expectedModCount = underlyingList.getModCount();
    }
    public IteratorExternal(ModCountList<E> underlyingList){this(underlyingList, 0);}
    
    public boolean hasNext() {
        return cursor != underlyingList.size();
    }

    public E next() {
        checkForComodification();
        try {
            E next = underlyingList.get(cursor);
            lastRet = cursor;
            cursor++;
            return next;
        } catch (IndexOutOfBoundsException e) {
            checkForComodification();
            throw new NoSuchElementException();
        }
    }

    public void remove() {
        if (lastRet < 0)
            throw new IllegalStateException();
        checkForComodification();

        try {
        	underlyingList.remove(lastRet);
            if (lastRet < cursor)
                cursor--;
            lastRet = -1;
            expectedModCount = underlyingList.getModCount();
        } catch (IndexOutOfBoundsException e) {
            throw new ConcurrentModificationException();
        }
    }

    protected final void checkForComodification() {
        if (underlyingList.getModCount() != expectedModCount)
            throw new ConcurrentModificationException();
    }
}
