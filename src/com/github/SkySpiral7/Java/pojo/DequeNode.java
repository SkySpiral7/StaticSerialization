package com.github.SkySpiral7.Java.pojo;

import java.util.Objects;

/**
 * This is mutable class for the nodes of a Deque. It keeps itself linked together but is not thread safe.
 * There is only 1 public constructor, for other ways to create notes use the factory.
 *
 * @param <E> the data type to be stored
 * @see Factory
 */
public class DequeNode<E> {
    /**
     * the data that this node holds
     */
	protected E data;
    /**
     * the next node or null if there is no next node
     */
	protected DequeNode<E> next;
	/**
     * the previous node or null if there is no previous node
     */
	protected DequeNode<E> prev;

    /**
     * Create a stand alone node that does not have a previous or next node.
     * This is the only public constructor because there is nothing to link together.
     * For other ways to create nodes use the Factory.
     * @see Factory
     */
    public DequeNode(E data){this(null, data, null);}
    /**
     * A short hand to set the fields. Linking can only be done in Factory because I can't use this in a constructor.
     */
    protected DequeNode(DequeNode<E> prev, E data, DequeNode<E> next) {
		this.data = data;
        this.next = next;
        this.prev = prev;
    }

    /**
     * Removes this node from the list. The neighboring nodes are linked together and this node becomes stand alone (no neighbors).
     * @return itself
     */
    public DequeNode<E> remove() {
	    if(prev != null) prev.next = next;
	    if(next != null) next.prev = prev;
	    prev = next = null;
	    return this;
    }

    /**
     * Note that this simply calls data.toString(). Next and previous nodes are not included to prevent the entire list
     * being evaluated. This implementation also allows the linked list to call node.toString for each element.
     */
    @Override
    public String toString(){return data.toString();}

    /**
     * Note that this simply calls data.hashCode(). Next and previous nodes are not included to prevent the entire list
     * being evaluated. This implementation also allows the linked list to call node.hashCode for each element.
     */
    @Override
    public int hashCode(){return data.hashCode();}

    /**
     * Note that this only compares data.equals(). Next and previous nodes are not included to prevent the entire list
     * being evaluated. This implementation also allows the linked list to call node.equals for each element.
     */
    @Override
    public boolean equals(Object obj) {
    	if(this == obj) return true;
    	if(!(obj instanceof DequeNode)) return false;  //this includes null check and child classes
    	return Objects.equals(data, (((DequeNode<?>) obj).data));
    }

    /**
     * @return the data that this node holds
     */
    public E getData(){return data;}

    /**
     * @return the next node or null if there is no next node
     */
    public DequeNode<E> getNext(){return next;}

    /**
     * @return the previous node or null if there is no previous node
     */
    public DequeNode<E> getPrev(){return prev;}

    /**
     * @param the data that this node holds
     */
    public void setData(E data){this.data = data;}

    //TODO: add public methods for insertion

    /**
     * This factory creates nodes and links them together.
     */
	public static class Factory {
	    /**
	     * Create a stand alone node that does not have a previous or next node.
	     * @see DequeNode
	     * @see #createNodeBetween(DequeNode, Object, DequeNode)
	     */
		public static <E> DequeNode<E> createStandAloneNode(E data){return new DequeNode<E>(data);}

	    /**
		 * Insert a new node that comes before the node given.
		 * @return the newly created node
		 * @see #createNodeBetween(DequeNode, Object, DequeNode)
		 */
		public static <E> DequeNode<E> createNodeBefore(E data, DequeNode<E> next) {
			DequeNode<E> prev = null;
			if(next != null) prev = next.getPrev();
			return createNodeBetween(prev, data, next);
		}

		/**
		 * Prev and next will both be linked to the new node and the new node will be linked to each of them.
		 *
		 * @param prev the node that comes before this node or null if there is no previous node
		 * @param data the data that this node holds
		 * @param next the node that comes after this node or null if there is no next node
		 * @return the newly created node
		 */
		public static <E> DequeNode<E> createNodeBetween(DequeNode<E> prev, E data, DequeNode<E> next) {
			DequeNode<E> newNode = new DequeNode<E>(prev, data, next);

		    if(prev != null) prev.next = newNode;
		    if(next != null) next.prev = newNode;

		    return newNode;
		}

		/**
		 * Insert a new node that comes after the node given.
		 * @return the newly created node
	     * @see #createNodeBetween(DequeNode, Object, DequeNode)
	     */
		public static <E> DequeNode<E> createNodeAfter(DequeNode<E> prev, E data) {
			DequeNode<E> next = null;
			if(prev != null) next = prev.getNext();
			return createNodeBetween(prev, data, next);
		}
	}

}
