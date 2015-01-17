package src.defaultImplementations;

/**
 * This is mutable bean for the nodes of a Deque. It is not thread safe.
 * @param <E> the data type to be stored
 */
public class DequeNode<E> {
    /**
     * the data that this node holds
     */
    private E data;
    /**
     * the next node or null if there is no next node
     */
    private DequeNode<E> next;
	/**
     * the previous node or null if there is no previous node
     */
    private DequeNode<E> prev;

    /**
     * Create a stand alone node that does not have a previous or next node.
     * @see #DequeNode(DequeNode, Object, DequeNode)
     */
    public DequeNode(E data){this(null, data, null);}
    /**
     * Create a last node that does not have a next node.
     * @see #DequeNode(DequeNode, Object, DequeNode)
     */
    public DequeNode(DequeNode<E> prev, E data){this(prev, data, null);}
    /**
     * Create a first node that does not have a previous node.
     * @see #DequeNode(DequeNode, Object, DequeNode)
     */
    public DequeNode(E data, DequeNode<E> next){this(null, data, next);}
    /**
     * @param prev the node that comes before this node or null if there is no previous node
     * @param data the data that this node holds
     * @param next the node that comes after this node or null if there is no next node
     */
    public DequeNode(DequeNode<E> prev, E data, DequeNode<E> next) {
        this.data = data;
        this.next = next;
        this.prev = prev;
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
    	return data.equals(((DequeNode<?>) obj).data);
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

    /**
     * @param the next node or null if there is no next node
     */
	public void setNext(DequeNode<E> next){this.next = next;}

	/**
     * @param the previous node or null if there is no previous node
     */
	public void setPrev(DequeNode<E> prev){this.prev = prev;}

}
