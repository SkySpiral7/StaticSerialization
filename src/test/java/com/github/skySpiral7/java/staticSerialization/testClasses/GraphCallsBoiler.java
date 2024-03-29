package com.github.skySpiral7.java.staticSerialization.testClasses;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.StaticSerializable;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public final class GraphCallsBoiler implements StaticSerializable
{
   private final Node root;

   public GraphCallsBoiler(final Node root)
   {
      this.root = root;
   }

   public static GraphCallsBoiler readFromStream(final ObjectStreamReader reader)
   {
      return new GraphCallsBoiler(reader.readObject(Node.class));
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
         unexplored.addAll(cursor.links);
      }
      return result;
   }

   @Override
   public boolean equals(final Object obj)
   {
      if (!(obj instanceof GraphCallsBoiler)) return false;
      final GraphCallsBoiler other = (GraphCallsBoiler) obj;
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
      public final List<Node> links;

      public Node(final String data)
      {
         Objects.requireNonNull(data);
         this.data = data;
         links = new ArrayList<>();
      }

      public static Node readFromStream(final ObjectStreamReader reader)
      {
         return StaticSerializable.readFromStream(reader, Node::createEmpty, GraphCallsBoiler.Node::populate);
      }

      private static Node createEmpty(final ObjectStreamReader reader)
      {
         return new Node(reader.readObject(String.class));
      }

      private static void populate(final ObjectStreamReader reader, final Node result)
      {
         final int linkSize = reader.readObject(int.class);
         for (int linkIndex = 0; linkIndex < linkSize; ++linkIndex)
         {
            result.links.add(reader.readObject(Node.class));
         }
      }

      @Override
      public void writeToStream(final ObjectStreamWriter writer)
      {
         writer.writeObject(data);
         writer.writeObject(links.size());
         links.forEach(writer::writeObject);
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
