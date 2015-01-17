package src;

import java.util.AbstractSequentialList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.ListIterator;

import src.defaultImplementations.DequeNode;

public class LinkedList<E> extends AbstractSequentialList<E> implements Deque<E> {
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
		DequeNode<E> next = null;
		if(prev != null) next = prev.getNext();
		DequeNode<E> newNode = DequeNode.Factory.createNodeBetween(prev, data, next);
		if(prev == null) first = newNode;  //insert first
		if(next == null) last = newNode;  //insert last
		//these also cover if list was empty. insert between is covered by the factory
		size++;
	}

	@Override
	public E removeFirst() {
		if(size == 0) throw new IllegalStateException("The list is empty. The first element can't be removed because it doesn't exist");
		return pollFirst();
	}

	@Override
	public E removeLast() {
		if(size == 0) throw new IllegalStateException("The list is empty. The last element can't be removed because it doesn't exist");
		return pollLast();
	}

	@Override
	public E pollFirst() {
		if(size == 0) return null;
		return removeNode(first);
	}

	@Override
	public E pollLast() {
		if(size == 0) return null;
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

	    return returnValue;
	}

	@Override
	public E getFirst() {
		return get(0);
	}

	@Override
	public E getLast() {
		return get(size-1);
	}

	@Override
	public E peekFirst() {
		if(size == 0) return null;
		return getFirst();
	}

	@Override
	public E peekLast() {
		if(size == 0) return null;
		return getLast();
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
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ListIterator<E> listIterator(int index) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int size() {
		return size;
	}

	/*
	JRE LinkedList also has these:
	add(E)
	add(int, E)
	addAll(int, Collection<? extends E>)
	addAll(Collection<? extends E>)
	clear()
	contains(Object)
	get(int)
	indexOf(Object)
	lastIndexOf(Object)
	remove(int)
	remove(Object)
	set(int, E)
	spliterator()
	toArray()
	toArray(T[])
	*/
}
