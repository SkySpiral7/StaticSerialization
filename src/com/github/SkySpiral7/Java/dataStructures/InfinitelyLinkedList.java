package com.github.SkySpiral7.Java.dataStructures;

import static com.github.SkySpiral7.Java.pojo.Comparison.GREATER_THAN;
import static com.github.SkySpiral7.Java.pojo.Comparison.GREATER_THAN_OR_EQUAL_TO;
import static com.github.SkySpiral7.Java.pojo.Comparison.LESS_THAN;
import static com.github.SkySpiral7.Java.pojo.Comparison.LESS_THAN_OR_EQUAL_TO;
import static com.github.SkySpiral7.Java.util.ComparableSugar.is;
import static com.github.SkySpiral7.Java.util.ComparableSugar.isComparisonResult;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.ListIterator;

import com.github.SkySpiral7.Java.InfiniteInteger;
import com.github.SkySpiral7.Java.ListIndexOutOfBoundsException;
import com.github.SkySpiral7.Java.iterators.DequeNodeIterator;
import com.github.SkySpiral7.Java.iterators.DescendingListIterator;
import com.github.SkySpiral7.Java.pojo.DequeNode;

public class InfinitelyLinkedList<E> extends LinkedList<E> {
	public static final int INVALID_SIZE = -1;

	protected InfiniteInteger actualSize;

	public InfinitelyLinkedList(){size = INVALID_SIZE; actualSize = InfiniteInteger.ZERO;}
    public InfinitelyLinkedList(Collection<? extends E> initialElements){this(); addAll(initialElements);}
    public InfinitelyLinkedList(E[] initialElements){this(Arrays.asList(initialElements));}

    @Override
	public boolean offerFirst(E newElement) {
		insertNodeAfter(null, newElement);
		return true;
	}

	@Override
	public boolean offerLast(E newElement) {
		insertNodeAfter(last, newElement);  //if(this.isEmpty()) then it will insert first
		return true;
	}

	@Override
	protected void insertNodeAfter(DequeNode<E> prev, E data) {
		super.insertNodeAfter(prev, data);
		size--;  //undo size++ but it's ok if modCount overflows
		actualSize = actualSize.add(1);
	}

	@Override
	protected E removeNode(DequeNode<E> nodeToRemove) {
		E returnValue = super.removeNode(nodeToRemove);
		size++;  //undo size-- but it's ok if modCount overflows
		actualSize = actualSize.subtract(1);
		return returnValue;
	}

	public ListIterator<E> listIterator(InfiniteInteger startingIndex) {
		rangeCheckForGet(startingIndex);
		ListIterator<E> returnValue = new DequeNodeIterator.IndexAgnosticValueIterator<E>(getNode(startingIndex));
		return returnValue;
	}

	public InfiniteInteger getActualSize() {
		return actualSize;
	}

	@Override
	public int size() {
		if(isComparisonResult(actualSize.compareTo(Integer.MAX_VALUE), GREATER_THAN)) return INVALID_SIZE;
		return actualSize.intValue();
	}

	@Override
	protected void rangeCheckForAdd(int index) {
		rangeCheckForAdd(InfiniteInteger.valueOf(index));
	}

	@Override
	protected void rangeCheckForGet(int index) {
		rangeCheckForGet(InfiniteInteger.valueOf(index));
	}

	protected void rangeCheckForAdd(InfiniteInteger index) {
		if(actualSize.equals(index)) return;
		rangeCheckForGet(index);
	}

	protected void rangeCheckForGet(InfiniteInteger index) {
		if(is(index, GREATER_THAN_OR_EQUAL_TO, actualSize)) throw new ListIndexOutOfBoundsException("Index: "+index+", Size: "+actualSize);
	}

    @Override
    public boolean add(E newElement) {
    	insertNodeAfter(last, newElement);
    	return true;
    }

    @Override
    public void add(int insertionIndex, E newElement) {
    	insertNodeAfter(getNode(insertionIndex), newElement);
    }

    public void add(InfiniteInteger insertionIndex, E newElement) {
    	insertNodeAfter(getNode(insertionIndex), newElement);
    }

    @Override
    public boolean addAll(int insertionIndex, Collection<? extends E> newElements) {
        return addAll(InfiniteInteger.valueOf(insertionIndex), newElements);
    }

    public boolean addAll(InfiniteInteger insertionIndex, Collection<? extends E> newElements) {
    	rangeCheckForAdd(insertionIndex);
    	boolean modified = false;
        Iterator<? extends E> newElementsIterator = newElements.iterator();
        DequeNode<E> insertAfterThisNode = getNode(insertionIndex);
        while (newElementsIterator.hasNext()) {
        	insertNodeAfter(insertAfterThisNode, newElementsIterator.next());
            modified = true;
        }
        return modified;
    }

    @Override
    public boolean addAll(Collection<? extends E> newElementCollection) {
    	boolean modified = false;
        for (E newElement : newElementCollection)
            if (add(newElement))
                modified = true;
        return modified;
    }

    public E get(InfiniteInteger index) {
		rangeCheckForGet(index);
		return getNode(index).getData();
    }

    //super.clear will work fine since it calls isEmpty and removeNode

    public boolean containsFromEnd(Object objectToFind) {
    	Iterator<E> it = descendingIterator();
        if (objectToFind==null) {
            while (it.hasNext())
                if (it.next()==null)
                    return true;
        } else {
            while (it.hasNext())
                if (objectToFind.equals(it.next()))
                    return true;
        }
        return false;
    }

    @Override
    public boolean removeFirstOccurrence(Object elementToRemove) {
        Iterator<DequeNode<E>> it = new DequeNodeIterator.IndexAgnosticDequeIterator<E>(first);
        if (elementToRemove==null)
        {
            while (it.hasNext())
            {
            	DequeNode<E> thisNode = it.next();
            	if (thisNode.getData()==null)
                {
                    removeNode(thisNode);
                	return true;
                }
            }
        }
        else
        {
            while (it.hasNext())
            {
            	DequeNode<E> thisNode = it.next();
                if (elementToRemove.equals(thisNode.getData()))
                {
                    removeNode(thisNode);
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public boolean removeLastOccurrence(Object elementToRemove) {
        Iterator<DequeNode<E>> it = DescendingListIterator.iterateBackwards(new DequeNodeIterator.IndexAgnosticDequeIterator<E>(last));
        if (elementToRemove==null)
        {
            while (it.hasNext())
            {
            	DequeNode<E> thisNode = it.next();
            	if (thisNode.getData()==null)
                {
                    removeNode(thisNode);
                	return true;
                }
            }
        }
        else
        {
            while (it.hasNext())
            {
            	DequeNode<E> thisNode = it.next();
                if (elementToRemove.equals(thisNode.getData()))
                {
                    removeNode(thisNode);
                    return true;
                }
            }
        }
        return false;
    }

	@Override
	public Iterator<E> descendingIterator() {
		return DescendingListIterator.iterateBackwards(new DequeNodeIterator.IndexAgnosticValueIterator<E>(last));
	}

	@Override
    public int lastIndexOf(Object objectToFind) {
		if(isComparisonResult(actualSize.compareTo(Integer.MAX_VALUE), GREATER_THAN)) throw new IllegalStateException("The list is larger than an Integer can represent.");
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

    public InfiniteInteger lastActualIndexOf(Object objectToFind) {
        InfiniteInteger index = actualSize;
        if (objectToFind == null) {
            for (DequeNode<E> currentNode = last; currentNode != null; currentNode = currentNode.getPrev()) {
            	index=index.subtract(1);
                if(currentNode.getData() == null) return index;
            }
        } else {
            for (DequeNode<E> currentNode = last; currentNode != null; currentNode = currentNode.getPrev()) {
            	index=index.subtract(1);
                if(objectToFind.equals(currentNode.getData())) return index;
            }
        }
        return InfiniteInteger.valueOf(ELEMENT_NOT_FOUND);
    }

    public E remove(InfiniteInteger index) {
    	DequeNode<E> nodeToRemove = getNode(index);
    	E returnValue = nodeToRemove.getData();
    	removeNode(nodeToRemove);
    	return returnValue;
    }

    @Override
    public DequeNode<E> getNode(int index) {
    	return getNode(InfiniteInteger.valueOf(index));
    }

    public DequeNode<E> getNode(InfiniteInteger index) {
    	rangeCheckForGet(index);

    	if (is(index, LESS_THAN_OR_EQUAL_TO, actualSize.divideByPowerOf2DropRemainder(1)))
    	{
    		DequeNode<E> currentNode = first;
            for(InfiniteInteger i = InfiniteInteger.ZERO; is(i, LESS_THAN, index); i=i.add(1))
            	currentNode = currentNode.getPrev();
            return currentNode;
        }
    	else
    	{
        	DequeNode<E> currentNode = last;
            for(InfiniteInteger i = actualSize.subtract(1); is(i, GREATER_THAN, index); i=i.subtract(1))
            	currentNode = currentNode.getPrev();
            return currentNode;
        }
    }

    @Override
    public E set(int index, E newValue) {
        return set(InfiniteInteger.valueOf(index), newValue);
    }

    public E set(InfiniteInteger index, E newValue) {
        DequeNode<E> nodeToChange = getNode(index);
        E oldValue = nodeToChange.getData();
        nodeToChange.setData(newValue);
        //doesn't increment modCount because there was no structural change
        return oldValue;
    }

    @Override
    public boolean isEmpty() {
    	return first == null;  //this is faster then checking size
    }

}
