package com.github.skySpiral7.java.staticSerialization.testClasses;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.StaticSerializable;

public final class GraphCallsReflection implements StaticSerializable
{
   private final Node root;

   public GraphCallsReflection(final Node root)
   {
      this.root = root;
   }

   public static GraphCallsReflection readFromStream(final ObjectStreamReader reader)
   {
      return new GraphCallsReflection(reader.readObject(Node.class));
   }

   @Override
   public void writeToStream(final ObjectStreamWriter writer)
   {
      writer.writeObject(root);
   }

   public Node getRoot()
   {
      return root;
   }

   private List<Node> getAllNodes()
   {
      //I can't use IdentityHashMap alone because I must retain order
      final List<Node> result = new ArrayList<>();
      final Map<Node, Boolean> visited = new IdentityHashMap<>();
      final Deque<Node> unexplored = new ArrayDeque<>();
      unexplored.add(root);
      while (!unexplored.isEmpty())
      {
         final Node cursor = unexplored.removeLast();
         if (visited.containsKey(cursor)) continue;
         result.add(cursor);
         visited.put(cursor, Boolean.TRUE);
         unexplored.add(cursor.next);
      }
      return result;
   }

   @Override
   public boolean equals(final Object obj)
   {
      if (!(obj instanceof GraphCallsReflection)) return false;
      final GraphCallsReflection other = (GraphCallsReflection) obj;
      final List<Node> allOtherNodes = other.getAllNodes();
      final List<Node> allMyNodes = this.getAllNodes();
      if (allOtherNodes.size() != allMyNodes.size()) return false;
      for (int i = 0; i < allMyNodes.size(); ++i)
      {
         if (!allMyNodes.get(i).equals(allOtherNodes.get(i))) return false;
      }
      return true;
   }

   @Override
   public int hashCode()
   {
      int hash = 3;
      final List<Node> allNodes = this.getAllNodes();
      for (final Node node : allNodes)
      {
         hash ^= node.hashCode();
      }
      return hash;
   }

   @Override
   public String toString()
   {
      final List<Node> allNodes = this.getAllNodes();
      return allNodes.toString();
   }

   public static final class Node implements StaticSerializable
   {
      public final String data;
      public Node next;

      public Node(final String data)
      {
         Objects.requireNonNull(data);
         this.data = data;
      }

      public static Node readFromStream(final ObjectStreamReader reader)
      {
         final Node result = new Node(reader.readObject(String.class));
         reader.readFieldsReflectively(result);
         return result;
      }

      @Override
      public void writeToStream(final ObjectStreamWriter writer)
      {
         if (writer.getObjectRegistry().shouldNotWrite(this, writer)) return;
         writer.writeObject(data);
         writer.writeFieldsReflectively(this);
      }

      @Override
      public boolean equals(final Object obj)
      {
         if (!(obj instanceof Node)) return false;
         return this.data.equals(((Node) obj).data);
      }

      @Override
      public int hashCode()
      {
         return data.hashCode();
      }

      @Override
      public String toString()
      {
         return data;
      }
   }
}
