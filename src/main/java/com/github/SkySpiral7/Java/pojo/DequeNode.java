package com.github.SkySpiral7.Java.pojo;

import java.io.Serializable;
import java.util.Objects;

import com.github.SkySpiral7.Java.dataStructures.LinkedList;
import com.github.SkySpiral7.Java.iterators.DequeNodeIterator;

/**
 * <p>This is mutable class for the nodes of a Deque. It keeps itself linked together but is not thread safe.
 * Nodes can be created with constructors or the factory.</p>
 *
 * <p>Note the scope and purpose of this class. This is a deque's node but not the {@link LinkedList deque itself}.
 * These are simple nodes without much functionality. Therefore a minimum number of public methods are provided
 * for a deque implementation. This is not designed for a tree or graph but a list: this is designed as a node
 * with previous node and next node pointers (and data) to be arranged in a line. This is not designed for complicated
 * insertions or sorting.</p>
 *
 * <p>This class's self linking nature should prevent pointer mismatch for linear structures.
 * This class does not validate pointers or detect structure shape.
 * Although not supported other shapes are possible (for example a circle should function mostly correctly).</p>
 *
 * @param <E> the data type to be stored
 *
 * @see Factory
 * @see DequeNodeIterator
 * @see LinkedList My Linked List which uses these nodes
 */
public class DequeNode<E> implements Serializable
{
   private static final long serialVersionUID = 1L;

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
    *
    * @see Factory#createStandAloneNode(Object)
    * @see #DequeNode(DequeNode, Object, DequeNode)
    */
   public DequeNode(E data){this.data = data;}

   /**
    * Create a new node that comes before the node given.
    *
    * @see Factory#createNodeBefore(DequeNode, Object)
    * @see #DequeNode(DequeNode, Object, DequeNode)
    */
   public DequeNode(E data, DequeNode<E> next)
   {
      this(data);
      DequeNode<E> prev = null;
      if (next != null) prev = next.getPrev();
      insertThisBetween(prev, next);
   }

   /**
    * Prev and next will both be linked to the new node and the new node will be linked to each of them.
    *
    * @param prev the node that comes before this node or null if there is no previous node
    * @param data the data that this node holds
    * @param next the node that comes after this node or null if there is no next node
    *
    * @return the newly created node
    *
    * @see Factory#createNodeBetween(DequeNode, Object, DequeNode)
    * @see #DequeNode(Object)
    */
   public DequeNode(DequeNode<E> prev, E data, DequeNode<E> next)
   {
      this(data);
      insertThisBetween(prev, next);
   }

   /**
    * Creates a new node that comes after the node given.
    *
    * @return the newly created node
    *
    * @see Factory#createNodeAfter(DequeNode, Object)
    * @see #DequeNode(DequeNode, Object, DequeNode)
    */
   public DequeNode(DequeNode<E> prev, E data)
   {
      this(data);
      DequeNode<E> next = null;
      if (prev != null) next = prev.getNext();
      insertThisBetween(prev, next);
   }

   /**
    * Prev and next will both be linked to this node and this node will be linked to each of them.
    * This method is where the node linking is done. It is simple and DRY.
    * This method is protected because this class is not a deque implementation.
    *
    * @see #DequeNode(DequeNode, Object, DequeNode)
    * @see DequeNode
    */
   protected void insertThisBetween(DequeNode<E> prev, DequeNode<E> next)
   {
      this.prev = prev;
      this.next = next;

      if (prev != null) prev.next = this;
      if (next != null) next.prev = this;
   }

   /**
    * Removes this node from the list. The neighboring nodes are linked together and this node becomes stand alone (no neighbors).
    *
    * @return itself
    */
   public DequeNode<E> remove()
   {
      if (prev != null) prev.next = this.next;
      if (next != null) next.prev = this.prev;
      this.prev = this.next = null;
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
   public boolean equals(Object obj)
   {
      if (this == obj) return true;
      if (!(obj instanceof DequeNode)) return false;  //this includes null check and child classes
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

   /**
    * This factory creates nodes and links them together.
    * Each of these methods simply call a public constructor
    * but the method names are more human readable.
    */
   public static class Factory
   {
      /**
       * Create a stand alone node that does not have a previous or next node.
       *
       * @see #createNodeBetween(DequeNode, Object, DequeNode)
       */
      public static <E> DequeNode<E> createStandAloneNode(E data){return new DequeNode<E>(data);}

      /**
       * Insert a new node that comes before the node given.
       *
       * @return the newly created node
       *
       * @see #createNodeBetween(DequeNode, Object, DequeNode)
       */
      public static <E> DequeNode<E> createNodeBefore(E data, DequeNode<E> next){return new DequeNode<E>(data, next);}

      /**
       * Prev and next will both be linked to the new node and the new node will be linked to each of them.
       *
       * @param prev the node that comes before this node or null if there is no previous node
       * @param data the data that this node holds
       * @param next the node that comes after this node or null if there is no next node
       *
       * @return the newly created node
       */
      public static <E> DequeNode<E> createNodeBetween(DequeNode<E> prev, E data, DequeNode<E> next)
      {
         return new DequeNode<E>(prev, data, next);
      }

      /**
       * Insert a new node that comes after the node given.
       *
       * @return the newly created node
       *
       * @see #createNodeBetween(DequeNode, Object, DequeNode)
       */
      public static <E> DequeNode<E> createNodeAfter(DequeNode<E> prev, E data){return new DequeNode<E>(prev, data);}
   }

}
