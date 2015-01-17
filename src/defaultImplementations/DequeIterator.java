package src.defaultImplementations;

import java.util.ListIterator;
import java.util.NoSuchElementException;
import java.util.Objects;

/**
 * This is a ListIterator for a Deque. It is not thread safe and assumes the deque won't be modified by another class.
 * It also assumes that the list will never have Integer.MAX_VALUE elements or more.
 * @param <E> the data type stored in the DequeNode
 */
public class DequeIterator<E> implements ListIterator<DequeNode<E>> {
	private DequeNode<E> nextNode;
	private int nextIndex;
	private DequeNode<E> previouslyVistedNode;
	private boolean amAtEnd;

	//not null. use Collections.emptyListIterator()
	//maybe allow empty so that add can become entire list
	public DequeIterator(DequeNode<E> startingNode, int startingIndex) {
		Objects.requireNonNull(startingNode);
		nextNode = startingNode;
		nextIndex = startingIndex;
		amAtEnd = false;
	}

	@Override
	public boolean hasNext() {
		return (nextNode != null && !amAtEnd);
	}

	@Override
	public DequeNode<E> next() {
		if(!this.hasNext()) throw new NoSuchElementException();
		previouslyVistedNode = nextNode;
		if(nextNode.getNext() != null) nextNode = nextNode.getNext();
		else amAtEnd = true;
		nextIndex++;
		return previouslyVistedNode;
	}

	@Override
	public boolean hasPrevious() {
		return (amAtEnd || (nextNode != null && nextNode.getPrev() != null));
	}

	@Override
	public DequeNode<E> previous() {
		if(!this.hasPrevious()) throw new NoSuchElementException();
		if(!amAtEnd) nextNode = nextNode.getPrev();
		else amAtEnd = false;
		nextIndex--;
		previouslyVistedNode = nextNode;
		return nextNode;
	}

	@Override
	public int nextIndex() {
		return nextIndex;
	}

	@Override
	public int previousIndex() {
		return (nextIndex-1);
	}

	@Override
	public void remove() {
		if(previouslyVistedNode == null) throw new IllegalStateException();

		if (previouslyVistedNode == nextNode)  //went Back
		{
			if(previouslyVistedNode.getNext() != null) nextNode = previouslyVistedNode.getNext();
			else
			{
				amAtEnd = true;
				nextNode = previouslyVistedNode.getPrev();
			}
		}
		else if(amAtEnd) nextNode = previouslyVistedNode.getPrev();  //went Forward
		previouslyVistedNode.remove();
		previouslyVistedNode = null;
		//TODO: test this complicated thing
	}

	@Override
	public void set(DequeNode<E> nodeWithNewData) {
		if(previouslyVistedNode == null) throw new IllegalStateException();
		if((nodeWithNewData.getPrev() != null && nodeWithNewData.getPrev() != previouslyVistedNode.getPrev()) ||
		   (nodeWithNewData.getNext() != null && nodeWithNewData.getNext() != previouslyVistedNode.getNext()))
			throw new UnsupportedOperationException("This iterator's set can only set the data, not change the order.");
			//this prevents the pointers from being mismatched
		previouslyVistedNode.setData(nodeWithNewData.getData());
	}

	@Override
	public void add(DequeNode<E> newNode) {
		if((newNode.getPrev() != null && newNode.getPrev() != nextNode) ||
		   (newNode.getNext() != null && newNode.getNext() != nextNode.getNext()))
			throw new UnsupportedOperationException("This iterator's add creates a new node given the data. But the node passed in had mismatching pointers.");
			//this prevents the pointers from being mismatched
		if(amAtEnd) nextNode = DequeNode.Factory.createNodeBefore(newNode.getData(), nextNode);
		else DequeNode.Factory.createNodeBefore(newNode.getData(), nextNode);
		previouslyVistedNode = null;
		nextIndex++;
	}

}
