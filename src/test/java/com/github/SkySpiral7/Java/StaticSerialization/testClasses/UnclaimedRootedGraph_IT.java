package com.github.SkySpiral7.Java.StaticSerialization.testClasses;

import java.io.File;

import com.github.SkySpiral7.Java.StaticSerialization.ObjectStreamReader;
import com.github.SkySpiral7.Java.StaticSerialization.ObjectStreamWriter;
import org.junit.Ignore;
import org.junit.Test;

import static org.junit.Assert.fail;

/**
 * Un-ignore this test to see how the "Failed to call claimId." exception looks in a normal use case.
 */
@Ignore
public class UnclaimedRootedGraph_IT
{
   @Test
   public void unclaimedRootedGraph() throws Exception
   {
      final File tempFile = File.createTempFile("UnclaimedRootedGraph_IT.TempFile.UnclaimedRootedGraph.", ".txt");
      tempFile.deleteOnExit();
      final UnclaimedRootedGraph graph;
      final UnclaimedRootedGraph.Node root = new UnclaimedRootedGraph.Node("Alice");
      {
         final UnclaimedRootedGraph.Node bob = new UnclaimedRootedGraph.Node("Bob");
         final UnclaimedRootedGraph.Node clark = new UnclaimedRootedGraph.Node("Clark");

         root.links.add(bob);
         bob.links.add(clark);
         clark.links.add(bob);
         clark.links.add(clark);
         //a -> b <-> c -> c

         graph = new UnclaimedRootedGraph(root);
      }

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(graph);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      reader.readObject(UnclaimedRootedGraph.class);
      fail("Should've thrown");
   }

   @Test
   public void rootNode() throws Exception
   {
      final File tempFile = File.createTempFile("UnclaimedRootedGraph_IT.TempFile.rootNode.", ".txt");
      tempFile.deleteOnExit();
      final UnclaimedRootedGraph.Node root = new UnclaimedRootedGraph.Node("Alice");
      {
         final UnclaimedRootedGraph.Node bob = new UnclaimedRootedGraph.Node("Bob");
         final UnclaimedRootedGraph.Node clark = new UnclaimedRootedGraph.Node("Clark");

         root.links.add(bob);
         bob.links.add(clark);
         clark.links.add(bob);
         clark.links.add(clark);
         //a -> b <-> c -> c
      }

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      //shows that self-referencing is handled as the root object being written
      writer.writeObject(root);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      reader.readObject();
      fail("Should've thrown");
   }

   @Test
   public void rootNode_allowsDirectCalling() throws Exception
   {
      final File tempFile = File.createTempFile("UnclaimedRootedGraph_IT.TempFile.rootNode_allowsDirectCalling.", ".txt");
      tempFile.deleteOnExit();
      final UnclaimedRootedGraph.Node root = new UnclaimedRootedGraph.Node("Alice");
      {
         final UnclaimedRootedGraph.Node bob = new UnclaimedRootedGraph.Node("Bob");
         final UnclaimedRootedGraph.Node clark = new UnclaimedRootedGraph.Node("Clark");

         root.links.add(bob);
         bob.links.add(clark);
         clark.links.add(bob);
         clark.links.add(clark);
         //a -> b <-> c -> c
      }

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      //shows that self-referencing is handled as the root object being written
      root.writeToStream(writer);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      UnclaimedRootedGraph.Node.readFromStream(reader);
      fail("Should've thrown");
   }
}
