package src;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.ListIterator;

import src.defaultImplementations.DequeNode;
import src.defaultImplementations.DequeNodeIterator;
import src.defaultImplementations.DescendingListIterator;

public class InfinitelyLinkedList<E> extends LinkedList<E> {
	public static final int INVALID_SIZE = -1;
	public static final BigInteger INVALID_SIZE_BIG_INTEGER = BigInteger.valueOf(INVALID_SIZE);
	protected final static BigInteger BIG_INTEGER_MAX_INT = BigInteger.valueOf(Integer.MAX_VALUE);

	protected BigInteger biggerSize;
	/**
	 * True if the list has more elements than the max of BigInteger. False otherwise.
	 */
	protected boolean sizeOverflow;

	public InfinitelyLinkedList(){size = INVALID_SIZE; biggerSize = BigInteger.ZERO; sizeOverflow = false;}
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
		changeSize(1);
	}

	protected void changeSize(int sizeChangeAmount) {
		biggerSize = changeBigInteger(biggerSize, sizeChangeAmount);
		if(biggerSize == INVALID_SIZE_BIG_INTEGER){sizeOverflow = true; biggerSize = BigInteger.ONE; return;}
		if(biggerSize.compareTo(BigInteger.ZERO) == 0) countSize();
		//TODO: is everything accounted for?
	}

	protected static BigInteger changeBigInteger(BigInteger bigIntToChange, int sizeChangeAmount) {
		try {
			return bigIntToChange.add(BigInteger.valueOf(sizeChangeAmount));
		}
		catch(Throwable e) {  //ArithmeticException (from overflow) or OutOfMemoryError etc
			return INVALID_SIZE_BIG_INTEGER;
		}
	}

	protected void countSize() {
		//TODO: add a constructor flag that prevents countSize
		BigInteger newSize = BigInteger.ZERO;
		Iterator<E> iter = iterator();
		while(iter.hasNext() && newSize != INVALID_SIZE_BIG_INTEGER)
			{newSize = changeBigInteger(newSize, 1); iter.next();}
		if(!iter.hasNext()){sizeOverflow = false; biggerSize = newSize;}
		else biggerSize = BigInteger.TEN;  //TODO: don't recount for 10?
	}

	@Override
	protected E removeNode(DequeNode<E> nodeToRemove) {
		E returnValue = super.removeNode(nodeToRemove);
		size++;  //undo size-- but it's ok if modCount overflows
		changeSize(-1);
		return returnValue;
	}

	public ListIterator<E> listIterator(BigInteger startingIndex) {
		rangeCheckForGet(startingIndex);
		ListIterator<E> returnValue = new DequeNodeIterator.IndexAgnosticValueIterator<E>(getNode(startingIndex));
		return returnValue;
	}

	public BigInteger getBigSize() {
		if(sizeOverflow) return INVALID_SIZE_BIG_INTEGER;
		return biggerSize;
	}

	@Override
	public int size() {
		if(sizeOverflow || biggerSize.compareTo(BIG_INTEGER_MAX_INT) == 1) return INVALID_SIZE;
		return biggerSize.intValue();
	}

	protected void rangeCheckForGet(BigInteger index) {
		if(sizeOverflow) return;
		if(biggerSize.compareTo(index) != 1) outOfBoundsMsg(index);
	}

	//add is ALWAYS possible by definition of this class
	@Override protected void rangeCheckForAdd(int index){}

	@Override
	protected void rangeCheckForGet(int index) {
		rangeCheckForGet(BigInteger.valueOf(index));
	}

    @Override
    protected String outOfBoundsMsg(int index) {
        return outOfBoundsMsg(BigInteger.valueOf(index));
    }

    protected String outOfBoundsMsg(BigInteger index) {
		//if(sizeOverflow) return "Index: "+index+", Size: Too Large";  //is never out of range
    	return "Index: "+index+", Size: "+biggerSize;
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

    public void add(BigInteger insertionIndex, E newElement) {
    	insertNodeAfter(getNode(insertionIndex), newElement);
    }

    @Override
    public boolean addAll(int insertionIndex, Collection<? extends E> newElements) {
        return addAll(BigInteger.valueOf(insertionIndex), newElements);
    }

    public boolean addAll(BigInteger insertionIndex, Collection<? extends E> newElements) {
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

    public E get(BigInteger index) {
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
        if(biggerSize.compareTo(BIG_INTEGER_MAX_INT) == 1) throw new IllegalStateException("The list is larger than an Integer can represent.");
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

    public BigInteger lastBigIndexOf(Object objectToFind) {
        if(sizeOverflow) throw new IllegalStateException("The list is larger than BigInteger can represent.");
        BigInteger index = biggerSize;
        if (objectToFind == null) {
            for (DequeNode<E> currentNode = last; currentNode != null; currentNode = currentNode.getPrev()) {
            	index=index.subtract(BigInteger.ONE);
                if(currentNode.getData() == null) return index;
            }
        } else {
            for (DequeNode<E> currentNode = last; currentNode != null; currentNode = currentNode.getPrev()) {
            	index=index.subtract(BigInteger.ONE);
                if(objectToFind.equals(currentNode.getData())) return index;
            }
        }
        return BigInteger.valueOf(ELEMENT_NOT_FOUND);
    }

    public E remove(BigInteger index) {
    	DequeNode<E> nodeToRemove = getNode(index);
    	E returnValue = nodeToRemove.getData();
    	removeNode(nodeToRemove);
    	return returnValue;
    }

    @Override
    public DequeNode<E> getNode(int index) {
    	return getNode(BigInteger.valueOf(index));
    }

    public DequeNode<E> getNode(BigInteger index) {
    	rangeCheckForGet(index);

    	if (!sizeOverflow && biggerSize.shiftRight(1).compareTo(index) == -1)
    	{
        	DequeNode<E> currentNode = last;
            for(BigInteger i = biggerSize.subtract(BigInteger.ONE); i.compareTo(index) == 1; i=i.subtract(BigInteger.ONE))
            	currentNode = currentNode.getPrev();
            return currentNode;
        }
    	else  //if size is unknown then we must count from the beginning
    	{
    		DequeNode<E> currentNode = first;
            for(BigInteger i = BigInteger.ZERO; i.compareTo(index) == -1; i=i.add(BigInteger.ONE))
            	currentNode = currentNode.getPrev();
            return currentNode;
        }
    }

    @Override
    public E set(int index, E newValue) {
        return set(BigInteger.valueOf(index), newValue);
    }

    public E set(BigInteger index, E newValue) {
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
