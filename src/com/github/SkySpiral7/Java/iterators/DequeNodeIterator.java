package com.github.SkySpiral7.Java.iterators;

import java.util.ListIterator;
import java.util.NoSuchElementException;
import java.util.Objects;

import com.github.SkySpiral7.Java.pojo.DequeNode;

/**
 * This is a ListIterator for a Deque. It is not thread safe and assumes the deque won't be modified by another class.
 * It also assumes that the list will never have Integer.MAX_VALUE elements or more.
 * @param <E> the data type stored in the DequeNode
 */
public class DequeNodeIterator<E> implements ListIterator<DequeNode<E>> {
	protected DequeNode<E> nextNode;
	protected int nextIndex;
	protected DequeNode<E> previouslyVistedNode;
	protected boolean amAtEnd;

	//not null. use Collections.emptyListIterator()
	//allow empty so that add can become entire list (as per doc)
	public DequeNodeIterator(DequeNode<E> startingNode, int startingIndex) {
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
		if(amAtEnd) nextNode = DequeNode.Factory.createNodeAfter(nextNode, newNode.getData());
		else DequeNode.Factory.createNodeBefore(newNode.getData(), nextNode);
		previouslyVistedNode = null;
		nextIndex++;
	}

	public static class ValueIterator<E> implements ListIterator<E> {
		protected DequeNodeIterator<E> underlyingIterator;

		public ValueIterator(DequeNode<E> startingNode, int startingIndex){underlyingIterator = new DequeNodeIterator<E>(startingNode, startingIndex);}
		public ValueIterator(DequeNodeIterator<E> underlyingIterator){this.underlyingIterator = underlyingIterator;}

		@Override public boolean hasNext(){return underlyingIterator.hasNext();}
		@Override public boolean hasPrevious() {return underlyingIterator.hasPrevious();}
		@Override public int nextIndex() {return underlyingIterator.nextIndex();}
		@Override public int previousIndex(){return underlyingIterator.previousIndex();}
		@Override public void remove(){underlyingIterator.remove();}

		@Override
		public E next() {
			return underlyingIterator.next().getData();
		}

		@Override
		public E previous() {
			return underlyingIterator.previous().getData();
		}

		@Override
		public void set(E newElementData) {
			underlyingIterator.set(new DequeNode<E>(newElementData));
		}

		@Override
		public void add(E newElementData) {
			underlyingIterator.add(new DequeNode<E>(newElementData));
		}
	}

	public static class IndexAgnosticDequeIterator<E> extends DequeNodeIterator<E> {
		public IndexAgnosticDequeIterator(DequeNode<E> startingNode) {
			super(startingNode, -1);
		}
		@Override
		public int nextIndex(){return -1;}
		@Override
		public int previousIndex(){return -1;}
	}

	/**
	 * This class iterates over the data of DequeNodes but does not know the index.
	 * Calling nextIndex or previousIndex will return -1. All other methods are
	 * the same as ValueIterator.
	 *
	 * @param <E> the type of element contained in the nodes
	 * @see IndexAgnosticDequeIterator
	 * @see ValueIterator
	 */
	public static class IndexAgnosticValueIterator<E> extends ValueIterator<E> {
		public IndexAgnosticValueIterator(DequeNode<E> startingNode) {
			super(startingNode, -1);
		}
		public IndexAgnosticValueIterator(DequeNodeIterator<E> underlyingIterator){super(underlyingIterator);}
		@Override
		public int nextIndex(){return -1;}
		@Override
		public int previousIndex(){return -1;}
	}

}
