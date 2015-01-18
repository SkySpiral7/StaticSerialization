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
	//TODO: maintaining this and size is stupid
	protected BigInteger biggerSize = BigInteger.ZERO;
	/**
	 * False if the list has more elements than maxBigInteger. True otherwise.
	 */
	protected boolean knownSize = true;

	public static final int INVALID_SIZE = -1;
	protected final static BigInteger maxLong = BigInteger.valueOf(Long.MAX_VALUE);
	protected final static BigInteger maxInt = BigInteger.valueOf(Integer.MAX_VALUE);
	protected static BigInteger maxUnsignedIntegerValue;
	protected static BigInteger bitsInAnIntegerArray;
	protected static BigInteger maxBigInteger;

	public InfinitelyLinkedList(){}
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

	protected void changeSize(int sizeChange) {
		//assert(sizeChange != 0); and that if sizeChange is negative it will not cause either size to be negative (if size is known)
		if(size != INVALID_SIZE && sizeChange < 0){size+=sizeChange; return;}
		if(size != INVALID_SIZE && size < (Integer.MAX_VALUE - sizeChange)){size+=sizeChange; return;}
		if(size != INVALID_SIZE){biggerSize = BigInteger.valueOf(size).add(BigInteger.valueOf(sizeChange)); size = INVALID_SIZE; return;}
		if(sizeChange > 0 && !bigIntegerWillLosePrecision(sizeChange)){biggerSize.add(BigInteger.valueOf(sizeChange)); return;}
		if(sizeChange > 0){biggerSize = BigInteger.ZERO; knownSize = false; return;}
		biggerSize.add(BigInteger.valueOf(sizeChange));
		if(biggerSize.signum() == -1) biggerSize = maxBigInteger;
		if(knownSize && biggerSize.compareTo(maxInt) == -1){size = biggerSize.intValue(); biggerSize = BigInteger.ZERO;}
	}

	protected boolean bigIntegerWillLosePrecision(int sizeChange) {
		if(biggerSize.compareTo(maxLong) != 1) return false;  //quick check
		if(maxUnsignedIntegerValue == null) maxUnsignedIntegerValue = BigInteger.valueOf(2).pow(32).subtract(BigInteger.ONE);
		//if(biggerSize.compareTo(maxUnsignedIntegerValue) != 1) return false;  //always false because I already compared max long
		if(bitsInAnIntegerArray == null) bitsInAnIntegerArray = maxUnsignedIntegerValue.multiply(BigInteger.valueOf(31)).subtract(BigInteger.ONE);
		if(biggerSize.compareTo(bitsInAnIntegerArray) != 1) return false;
		if(maxBigInteger == null) calculateMaxBigInteger();
		return (maxBigInteger.subtract(biggerSize).compareTo(BigInteger.valueOf(sizeChange)) == 1);
	}

	//TODO: double check the edge
	protected static void calculateMaxBigInteger() {
		maxBigInteger = BigInteger.valueOf(2);
		BigInteger exponentRemaining = bitsInAnIntegerArray;
		while (exponentRemaining.compareTo(BigInteger.TEN) == 1)
		{
			maxBigInteger.shiftLeft(10);
			exponentRemaining.subtract(BigInteger.TEN);
		}
		while (exponentRemaining.signum() == 1)
		{
			maxBigInteger.shiftLeft(1);
			exponentRemaining.subtract(BigInteger.ONE);
		}
		maxBigInteger.subtract(BigInteger.ONE);
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
		if(!knownSize) return BigInteger.valueOf(INVALID_SIZE);
		if(size != INVALID_SIZE) return BigInteger.valueOf(size);
		return biggerSize;
	}

	protected void rangeCheckForGet(BigInteger index) {
		if(!knownSize) return;
		BigInteger actualSize = getBigSize();
		if(actualSize.compareTo(index) != 1) outOfBoundsMsg(index);
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
        return "Index: "+index+", Size: "+getBigSize();
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

    //super.clear will work fine since removeNode will eventually make size == 0

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
        if(size == INVALID_SIZE) throw new IllegalStateException("The list is larger than an Integer can represent.");
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
        if(!knownSize) throw new IllegalStateException("The list is larger than BigInteger can represent.");
        BigInteger index = getBigSize();
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

    	if (knownSize && getBigSize().shiftRight(1).compareTo(index) == -1)
    	{
        	DequeNode<E> currentNode = last;
            for(BigInteger i = getBigSize().subtract(BigInteger.ONE); i.compareTo(index) == 1; i=i.subtract(BigInteger.ONE))
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

}
