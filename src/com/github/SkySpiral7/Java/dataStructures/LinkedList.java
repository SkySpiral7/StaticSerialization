package com.github.SkySpiral7.Java.dataStructures;

import java.util.AbstractSequentialList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.ListIterator;

import com.github.SkySpiral7.Java.Copyable;
import com.github.SkySpiral7.Java.ListIndexOutOfBoundsException;
import com.github.SkySpiral7.Java.iterators.DequeNodeIterator;
import com.github.SkySpiral7.Java.iterators.DescendingListIterator;
import com.github.SkySpiral7.Java.pojo.DequeNode;

//yes some things were copied from JRE stuff
//consider copying JRE LL read/write object.
//useless crap in JRE LL: linkfirst/last, unlinkfirst/last, is/checkindex
public class LinkedList<E> extends AbstractSequentialList<E> implements Deque<E>, ModCountList<E>, Copyable<LinkedList<E>> {
	public static final int ELEMENT_NOT_FOUND = - 1;

	/**
     * The size of the list is stored so that this.size() is O(1) complexity and
     * this.get(int) can check if the index is out of bounds without iteration.
     */
	protected int size;
    /**
     * A pointer to the first node or null if the list is empty
     * If the list has 1 element then first == last.
     */
	protected DequeNode<E> first;
    /**
     * A pointer to the last node or null if the list is empty
     * If the list has 1 element then first == last.
     */
    protected DequeNode<E> last;

    /**
     * Constructs an empty list.
     */
    public LinkedList(){}
    /**
     * Constructs a list containing the elements of the specified
     * collection, in the order they are returned by the collection's
     * iterator.
     *
     * @param  initialElements the collection whose elements are to be placed into this list
     * @throws NullPointerException if the specified collection is null
     */
    public LinkedList(Collection<? extends E> initialElements){this(); addAll(initialElements);}
    /**
     * Constructs a list containing the elements of the specified
     * array in the same order.
     *
     * @param  initialElements the collection whose elements are to be placed into this list
     * @throws NullPointerException if the specified collection is null
     */
    public LinkedList(E[] initialElements){this(Arrays.asList(initialElements));}

	@Override
	public void addFirst(E newElement) {
		if(!this.offerFirst(newElement)) throw new IllegalStateException("Capacity violating. The maximum number of elements can't exceed Integer.MAX_VALUE.");
	}

	@Override
	public void addLast(E newElement) {
		if(!this.offerLast(newElement)) throw new IllegalStateException("Capacity violating. The maximum number of elements can't exceed Integer.MAX_VALUE.");
	}

	@Override
	public boolean offerFirst(E newElement) {
		if(size == Integer.MAX_VALUE) return false;
		insertNodeAfter(null, newElement);
		return true;
	}

	@Override
	public boolean offerLast(E newElement) {
		if(size == Integer.MAX_VALUE) return false;
		insertNodeAfter(last, newElement);  //if(this.isEmpty()) then it will insert first
		return true;
	}

	protected void insertNodeAfter(DequeNode<E> prev, E data) {
		//assert(size != Integer.MAX_VALUE);
		DequeNode<E> next;
		if(prev == null) next = first;
		else next = prev.getNext();
		DequeNode<E> newNode = DequeNode.Factory.createNodeBetween(prev, data, next);
		if(prev == null) first = newNode;  //insert first
		if(next == null) last = newNode;  //insert last
		//these also cover if list was empty. insert between is covered by the factory
		size++;
		modCount++;
	}

	@Override
	public E removeFirst() {
		if(isEmpty()) throw new IllegalStateException("The list is empty. The first element can't be removed because it doesn't exist");
		return removeNode(first);
	}

	@Override
	public E removeLast() {
		if(isEmpty()) throw new IllegalStateException("The list is empty. The last element can't be removed because it doesn't exist");
		return removeNode(last);
	}

	@Override
	public E pollFirst() {
		if(isEmpty()) return null;
		return removeNode(first);
	}

	@Override
	public E pollLast() {
		if(isEmpty()) return null;
		return removeNode(last);
	}

	protected E removeNode(DequeNode<E> nodeToRemove) {
		//assert(!this.empty() && nodeToRemove != null);
		E returnValue = nodeToRemove.getData();
	    DequeNode<E> before = nodeToRemove.getPrev();
	    DequeNode<E> after = nodeToRemove.getNext();

	    if(before == null) first = after;  //since the first node is being removed
	    if(after == null) last = before;  //since the last node is being removed
	    nodeToRemove.remove();
		size--;
		modCount++;

	    return returnValue;
	}

	@Override
	public E getFirst() {
		rangeCheckForGet(0);  //checks for not empty
		return first.getData();
	}

	@Override
	public E getLast() {
		rangeCheckForGet(0);  //checks for not empty
		return last.getData();
	}

	@Override
	public E peekFirst() {
		if(isEmpty()) return null;
		return first.getData();
	}

	@Override
	public E peekLast() {
		if(isEmpty()) return null;
		return last.getData();
	}

	@Override
	public boolean removeFirstOccurrence(Object elementToRemove) {
		int index = this.indexOf(elementToRemove);
		if(index == ELEMENT_NOT_FOUND) return false;
		remove(index);
		return true;
	}

	@Override
	public boolean removeLastOccurrence(Object elementToRemove) {
		int index = this.lastIndexOf(elementToRemove);
		if(index == ELEMENT_NOT_FOUND) return false;
		remove(index);
		return true;
	}

	@Override
	public boolean offer(E newElement) {
		return offerLast(newElement);
	}

	@Override
	public E remove() {
		return removeFirst();
	}

	@Override
	public E poll() {
		return pollFirst();
	}

	@Override
	public E element() {
		return getFirst();
	}

	@Override
	public E peek() {
		return peekFirst();
	}

	@Override
	public void push(E newElement) {
		addFirst(newElement);
	}

	@Override
	public E pop() {
		return removeFirst();
	}

	@Override
	public Iterator<E> descendingIterator() {
		return DescendingListIterator.iterateBackwards(new DequeNodeIterator.ValueIterator<E>(last, size));
	}

	@Override
	public ListIterator<E> listIterator(int startingIndex) {
		rangeCheckForGet(startingIndex);
		ListIterator<E> returnValue = new DequeNodeIterator.ValueIterator<E>(getNode(startingIndex), startingIndex);
		return returnValue;
	}

	@Override
	public int size() {
		return size;
	}

    protected void rangeCheckForGet(int index) {
        if(index < 0 || index >= size()) throw new ListIndexOutOfBoundsException(index, size());
    }

    protected void rangeCheckForAdd(int index) {
    	if(index == size) return;
    	rangeCheckForGet(index);
    }

    @Override
    public boolean add(E newElement) {
		if(size == Integer.MAX_VALUE) return false;
    	insertNodeAfter(last, newElement);
    	return true;
    }

    @Override
    public void add(int insertionIndex, E newElement) {
    	if(size == Integer.MAX_VALUE) return;
        //if(insertionIndex == size){this.addLast(newElement); return;}
        //if(insertionIndex == 0){this.addFirst(newElement); return;}
    	//calling getNode is more efficient.
    	insertNodeAfter(getNode(insertionIndex).getPrev(), newElement);
    }

    @Override
    public boolean addAll(int insertionIndex, Collection<? extends E> newElements) {
    	if(size <= (Integer.MAX_VALUE - newElements.size())) return false;  //must use subtraction to prevent overflow
    	rangeCheckForAdd(insertionIndex);
        boolean modified = false;
        Iterator<? extends E> newElementsIterator = newElements.iterator();
        DequeNode<E> insertAfterThisNode = getNode(insertionIndex).getPrev();
        while (newElementsIterator.hasNext()) {
        	insertNodeAfter(insertAfterThisNode, newElementsIterator.next());
            modified = true;
        }
        return modified;
    }

    @Override
    public boolean addAll(Collection<? extends E> newElements) {
    	if(size <= (Integer.MAX_VALUE - newElements.size())) return false;  //must use subtraction to prevent overflow
    	return super.addAll(newElements);
    }

    @Override
    public void clear() {
    	//unlinking all the nodes is cargo cult. the garbage collector can handle it
    	first = last = null;
    	size = 0;
    	modCount++;
    }

    @Override
    public E get(int index) {
		rangeCheckForGet(index);
		return getNode(index).getData();
    }

    @Override
    public int lastIndexOf(Object objectToFind) {
        int index = size;
        if (objectToFind == null) {
            for (DequeNode<E> currentNode = last; currentNode != null; currentNode = currentNode.getPrev()) {
                index--;
                if(currentNode.getData() == null) return index;
            }
        } else {
            for (DequeNode<E> currentNode = last; currentNode != null; currentNode = currentNode.getPrev()) {
                index--;
                if(objectToFind.equals(currentNode.getData())) return index;
            }
        }
        return ELEMENT_NOT_FOUND;
    }

    @Override
    public E remove(int index) {
    	DequeNode<E> nodeToRemove = getNode(index);
    	E returnValue = nodeToRemove.getData();
    	removeNode(nodeToRemove);
    	return returnValue;
    }

    public DequeNode<E> getNode(int index) {
    	rangeCheckForGet(index);

    	if (index < (size >> 1)) {
    		DequeNode<E> currentNode = first;
            for(int i = 0; i < index; i++) currentNode = currentNode.getNext();
            return currentNode;
        } else {
        	DequeNode<E> currentNode = last;
            for(int i = size - 1; i > index; i--) currentNode = currentNode.getPrev();
            return currentNode;
        }
    }

    @Override
    public E set(int index, E newValue) {
        DequeNode<E> nodeToChange = getNode(index);
        E oldValue = nodeToChange.getData();
        nodeToChange.setData(newValue);
        //doesn't increment modCount because there was no structural change
        return oldValue;
    }

    @Override
    public Object[] toArray() {
        Object[] result = new Object[size];
        int i = 0;
        for (DequeNode<E> cursor = first; cursor != null; cursor = cursor.getNext())
    		{result[i] = cursor.getData(); i++;}
        return result;
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T[] toArray(T[] destination) {
    	if (destination.length < size)
            destination = (T[])java.lang.reflect.Array.newInstance(
                                destination.getClass().getComponentType(), size);
        int i = 0;
        //result exists in order to cause an ArrayStoreException instead of a ClassCastException
        Object[] result = destination;
        for (DequeNode<E> cursor = first; cursor != null; cursor = cursor.getNext())
        	{result[i] = cursor.getData(); i++;}

        if (destination.length > size)
            destination[size] = null;

        return destination;
    }

    @Override
	public int getModCount() {
		return modCount;
	}
	@Override
	public LinkedList<E> copy() {
		return new LinkedList<E>(this);  //acts as a copy constructor
	}

    //uses super.isEmpty() in AbstractCollection
    //uses super.containsAll() in AbstractCollection
    //uses super.removeAll(Collection<?>) in AbstractCollection
    //uses super.retainAll(Collection<?>) in AbstractCollection
    //uses super.remove(Object) in AbstractCollection
    //uses super.contains(Object) in AbstractCollection
    //uses super.iterator() in AbstractSequentialList
    //uses super.indexOf(Object) from AbstractList
    //JRE LinkedList also has spliterator()

}
