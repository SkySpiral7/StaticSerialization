package src.sameobject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

/**
 * <p>This list (as the name implies) implements functions based on the object being the same (using
 * pointer equality eg ==) instead of using .equals(). Therefore this class violates some of the contract
 * of List and Collection by design.</p>
 * 
 * <p>This class extends ArrayList and does not override the following methods of List:
 * size, isEmpty, iterator(all of them), toArray(both), add(all of them),
 * clear, get, set, constructors(sort of), hashCode, remove(int).</p>
 * 
 * <p>Note that this class can't be cloned or serialized and modifications done to a sublist
 * will not affect the original list.</p>
 */
public class SameObjectList<E> extends ArrayList<E> {
	private static final long serialVersionUID = 1L;

	public static final int ELEMENT_NOT_FOUND = - 1;

	/**
	 * The no-arg construct creates an empty list.
	 */
	public SameObjectList(){}
	/**
	 * Creates an empty list. Simply calls super(int).
	 * 
	 * @param initialCapacity the internal array's initial capacity 
	 * @see ArrayList#ArrayList(int)
	 */
	public SameObjectList(int initialCapacity){super(initialCapacity);}
    /**
     * Constructs a list containing the elements of the specified
     * collection, in the order they are returned by the collection's
     * iterator.
     *
     * @param initialElements the collection whose elements are to be placed into this list
     * @throws NullPointerException if initialElements is null
     * @see ArrayList#ArrayList(Collection)
     */
	public SameObjectList(Collection<? extends E> initialElements){super(initialElements);}
    /**
     * Constructs a list containing the elements of the specified
     * array, in the same order.
     *
     * @param initialElements the array whose elements are to be placed into this list
     * @throws NullPointerException if initialElements is null
     * @see ArrayList#ArrayList(Collection)
     */
	public SameObjectList(E[] initialElements){super(Arrays.asList(initialElements));}

    /**
     * <p>Returns <tt>true</tt> if this list contains the exact same object specified.
     * More formally, returns <tt>true</tt> if and only if this list contains
     * at least one <tt>element</tt> such that <tt>(objectToFind&nbsp;==&nbsp;element)</tt>.</p>
     *
     * <p>Note that this breaks the contract of Collection because it checks for pointer equality
     * to compare elements instead of using <code>.equals()</code>.</p>
     *
     * @param objectToFind element whose presence in this list is to be tested
     * @return <tt>true</tt> if this list contains objectToFind
     */
	@Override
	public boolean contains(Object objectToFind) {
		return this.indexOf(objectToFind) != ELEMENT_NOT_FOUND;
	}

    /**
     * <p>This implementation iterates over collectionToSearch,
     * calling <code>this.contains()</code> for each element returned by the iterator.
     * If all elements of collectionToSearch are contained in this list then
     * <tt>true</tt> is returned, otherwise <tt>false</tt>.</p>
     * 
     * <p>Note that <code>this.contains()</code> violates the contract of Collection. See their Javadocs for
     * more information.</p>
     *
     * @param  collectionToSearch collection to be checked for containment in this collection
     * @return <tt>true</tt> if this collection contains all of the elements
     *         in collectionToSearch
     * @throws NullPointerException if collectionToSearch is null.
     * @see #contains(Object)
     */
	@Override
	public boolean containsAll(Collection<?> collectionToSearch) {
		for(Object objectToFind : collectionToSearch)
		{
			if(!this.contains(objectToFind)) return false;
		}
		return true;
	}

    /**
     * <p>This method removes a single instance of objectToRemove from this
     * list, if it is present. More formally,
     * removes the first <tt>element</tt> such that <tt>(objectToRemove&nbsp;==&nbsp;element)</tt>.
     * Returns <tt>true</tt> if this collection contained the specified element (or
     * equivalently, if this collection changed as a result of the call).</p>
     *
     * <p>Note that this breaks the contract of Collection because it checks for pointer equality
     * to compare elements instead of using <code>.equals()</code>.</p>
     *
     * @param objectToRemove element to be removed from this collection, if present
     * @return <tt>true</tt> if an element was removed as a result of this call
     */
	@Override
	public boolean remove(Object objectToRemove) {
		int index = this.indexOf(objectToRemove);
		if(index == ELEMENT_NOT_FOUND) return false;
		remove(index);
		return true;
	}

    /**
     * <p>Removes all of this list's elements that are also contained in
     * collectionToRemove. After this call returns,
     * this collection will contain no elements in common with collectionToRemove.</p>
     *
     * <p>Note that this breaks the contract of Collection because it checks for pointer equality
     * to compare elements instead of using <code>.equals()</code>.</p>
     *
     * @param collectionToRemove collection containing elements to be removed from this collection
     * @return <tt>true</tt> if this collection changed as a result of the
     *         call
     * @throws NullPointerException if collectionToRemove is null
     * @see #remove(Object)
     */
	@Override
	public boolean removeAll(Collection<?> collectionToRemove) {
		boolean hasChanged = false;
		for(Object objectToRemove : collectionToRemove)
		{
			hasChanged = this.remove(objectToRemove) || hasChanged;  //must be in this order to avoid short circuit
		}
		return hasChanged;
	}

    /**
     * <p>Retains only the elements in this list that are contained in the
     * specified collection. In other words, removes from this list all
     * of its elements that are not contained in the specified collection.</p>
     *
     * <p>After this method {@code this.size() <= collectionToRetain.size()}.
     * Notice that calling this method is not the same as calling clear then addAll
     * because this list will not gain any elements it does not currently have.</p>
     *
     * <p>Note that this breaks the contract of Collection because it checks for pointer equality
     * to compare elements instead of using <code>.equals()</code>.</p>
     *
     * @param collectionToRetain collection containing elements to be retained in this list
     * @return {@code true} if this list changed as a result of the call
     * @throws NullPointerException if collectionToRetain is null
     */
	@Override
	public boolean retainAll(Collection<?> collectionToRetain) {
		boolean hasChanged = false;
		Objects.requireNonNull(collectionToRetain);
		SameObjectList<?> listToRetain = new SameObjectList<>(collectionToRetain);
		for(int index=0; index < this.size(); index++)
		{
			if(!listToRetain.contains(this.get(index)))
			{
				hasChanged = true;
				this.remove(index);
				index--;  //to prevent skipping an element
			}
		}
		return hasChanged;
	}

    /**
     * <p>Returns the index of the first occurrence of the specified element
     * in this list, or -1 if this list does not contain the element.
     * More formally, returns the lowest <tt>index</tt> such that
     * <tt>(this.get(index)&nbsp;==&nbsp;objectToFind</tt>,
     * or -1 if there is no such index.</p>
     *
     * <p>Note that this breaks the contract of List because it checks for pointer equality
     * to compare elements instead of using <code>.equals()</code>.</p>
     */
	@Override
	public int indexOf(Object objectToFind) {
		for(int index=0; index < this.size(); index++)
		{
			if(this.get(index) == objectToFind) return index;
		}
		return ELEMENT_NOT_FOUND;
	}

    /**
     * <p>Returns the index of the last occurrence of the specified element
     * in this list, or -1 if this list does not contain the element.
     * More formally, returns the highest <tt>index</tt> such that
     * <tt>(this.get(index)&nbsp;==&nbsp;objectToFind</tt>,
     * or -1 if there is no such index.</p>
     *
     * <p>Note that this breaks the contract of List because it checks for pointer equality
     * to compare elements instead of using <code>.equals()</code>.</p>
     */
	@Override
	public int lastIndexOf(Object objectToFind) {
		for(int index=this.size(); index > 0; index--)
		{
			if(this.get(index-1) == objectToFind) return index;
		}
		return ELEMENT_NOT_FOUND;
	}

	//TODO: make a sublist, map entry, iterator. any more I can think of
	/**
     * <p>Returns a view of the portion of this list between the specified
     * {@code fromIndex}, inclusive, and {@code toIndex}, exclusive.  (If
     * {@code fromIndex} and {@code toIndex} are equal, the returned list is
     * empty.) This section of this list is put into a new SameObjectList therefore
     * changes to the returned list will not affect this list.</p>
     *
     * <p>Note that this breaks the contract of List because modifications to the sublist
     * are supposed to be reflected on the parent list. This is done because I have not
     * yet implemented a proper sublist.</p>
	 */
	@Override
	public List<E> subList(int fromIndex, int toIndex) {
		return new SameObjectList<>(super.subList(fromIndex, toIndex));
	}

    /**
     * <p>Compares the specified object with this list for equality.  Returns
     * {@code true} if and only if the specified object is also a list, both
     * lists have the same size, and all corresponding pairs of elements in
     * the two lists are the same object. (Using {@code (e1 == e2)})
     * In other words, two lists are defined to be
     * equal if they contain the same elements in the same order.</p>
     *
     * <p>Note that this breaks the contract of List because it checks for pointer equality
     * to compare elements instead of using <code>.equals()</code>.
     * Additionally this.equals(obj) is probably not the same as obj.equals(this)
     * since obj will probably use .equals for comparing elements.</p>
     *
     * @param obj the object to be compared for equality with this list
     * @return {@code true} if the specified object is equal to this list
     */
	@Override
	public boolean equals(Object obj) {
		if(obj == this) return true;
		if(!(obj instanceof List<?>)) return false;

		List<?> otherList = (List<?>) obj;
		if(this.size() != otherList.size()) return false;
		//I can't use this.containsAll because the elements must be in the same order
		for(int index=0; index < this.size(); index++)
		{
			if(this.get(index) != otherList.get(index)) return false;
		}
		return true;
	}

	/**
	 * <p>Always throws.</p>
	 * 
     * @throws CloneNotSupportedException cloning is not currently supported because it is
     *         harder to do then it should be.
     * @return
	 */
	@Override
	public Object clone() {
		throw new RuntimeException(new CloneNotSupportedException());
	}
	
	/**
	 * <p>The string returned is JSON therefore it may prove useful in some way beyond readability.
	 * The string returned has the class, hash code, and element data clearly labeled.</p>
	 */
	@Override
	public String toString() {
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append("{\"class\": \"");
		stringBuilder.append(this.getClass().getName());
		stringBuilder.append("\", \"hexHash\": \"");
		stringBuilder.append(Integer.toHexString(this.hashCode()));
		stringBuilder.append("\", \"data\": ");
		stringBuilder.append(super.toString());
		stringBuilder.append('}');
		return stringBuilder.toString();
	}

}
