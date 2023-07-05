package com.github.skySpiral7.java.staticSerialization;

import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import com.github.skySpiral7.java.staticSerialization.testClasses.GraphCallsRegister;
import com.github.skySpiral7.java.staticSerialization.testClasses.SimpleHappy;
import org.junit.Ignore;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

/**
 * This test covers reasons why the user shouldn't call the public methods directly. Useful validation messages are not possible and it
 * won't do what you want or will fail. Therefore this test is junit ignored and excluded from maven-surefire-plugin.
 */
@Ignore
public class DoNotDo_IT
{
   /**
    * This is the most dangerous test of all because it appears to work. It does not read/write a header which means that it won't be able
    * to handle null or ids (see other tests). It also means that you must read and write directly so that the overhead is symmetrically
    * missing. Doing so would require your own book keeping and an avoidance of null and ids which is not practical or intended.
    */
   @Test
   public void doNotCallDirectlyEvenIfSeemsToWork()
   {
      final ByteAppender mockFile = new ByteAppender();
      final SimpleHappy data = new SimpleHappy(10);

      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      data.writeToStream(writer);
      writer.close();

      try (final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes())))
      {
         final SimpleHappy actual = SimpleHappy.readFromStream(reader);
         assertEquals(data, actual);
      }
   }

   /**
    * Calling readFromStream directly does not read the header and therefore will fail for properly written streams (which include
    * overhead).
    */
   @Test
   public void doNotCallReadDirectly()
   {
      final ByteAppender mockFile = new ByteAppender();
      final SimpleHappy data = new SimpleHappy(10);

      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      try (final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes())))
      {
         final SimpleHappy actual = SimpleHappy.readFromStream(reader);
         assertEquals(data, actual);
      }
   }

   /**
    * Calling writeToStream directly does not write the header thus creating an invalid stream. Reading the stream correctly will fail (when
    * it tries to read the overhead).
    */
   @Test
   public void doNotCallWriteDirectly()
   {
      final ByteAppender mockFile = new ByteAppender();
      final SimpleHappy data = new SimpleHappy(10);

      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      data.writeToStream(writer);
      writer.close();

      try (final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes())))
      {
         final SimpleHappy actual = reader.readObject(SimpleHappy.class);
         assertEquals(data, actual);
      }
   }

   /**
    * Null is header only meaning that calling readFromStream directly (which has no header) means you can't read null.
    */
   @Test
   public void doNotCallDirectlyForNull()
   {
      final ByteAppender mockFile = new ByteAppender();

      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(null);
      //null.writeToStream(writer); is not possible in Java so it can't be symmetric
      writer.close();

      try (ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes())))
      {
         final SimpleHappy actual = SimpleHappy.readFromStream(reader);
         assertNull(actual);
      }
   }

   /**
    * Since direct calling writeToStream does not create a header (or register) the object written can't return the same object twice.
    */
   @Test
   public void doNotCallDirectlyForId()
   {
      final ByteAppender mockFile = new ByteAppender();
      final GraphCallsRegister graph;
      final GraphCallsRegister.Node root = new GraphCallsRegister.Node("Alice");
      {
         final GraphCallsRegister.Node bob = new GraphCallsRegister.Node("Bob");
         final GraphCallsRegister.Node clark = new GraphCallsRegister.Node("Clark");

         root.links.add(bob);
         bob.links.add(clark);
         clark.links.add(bob);
         clark.links.add(clark);
         //a -> b <-> c -> c

         graph = new GraphCallsRegister(root);
      }

      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(graph);
      //only above registers so would get same error if direct call twice
      //graph.writeToStream(writer);
      graph.writeToStream(writer);
      writer.close();

      try (final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes())))
      {
         final GraphCallsRegister firstActualGraph = reader.readObject();
         //final GraphCallsRegister firstActualGraph = GraphCallsRegister.readFromStream(reader);
         final GraphCallsRegister secondActualGraph = GraphCallsRegister.readFromStream(reader);
         assertSame(firstActualGraph, secondActualGraph);
      }
   }

   /**
    * Since direct calling writeToStream does not create a header (or register) the object written can't be referenced by another object.
    */
   @Test
   public void doNotCallDirectlyForCircleRoot()
   {
      final ByteAppender mockFile = new ByteAppender();
      final GraphCallsRegister.Node root = new GraphCallsRegister.Node("Alice");
      {
         final GraphCallsRegister.Node bob = new GraphCallsRegister.Node("Bob");
         final GraphCallsRegister.Node clark = new GraphCallsRegister.Node("Clark");

         root.links.add(bob);
         bob.links.add(clark);
         clark.links.add(bob);
         clark.links.add(clark);
         //a -> b <-> c -> c
      }

      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      root.writeToStream(writer);
      writer.close();

      try (final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes())))
      {
         final GraphCallsRegister.Node actualRoot = GraphCallsRegister.Node.readFromStream(reader);
         assertNotSame(root, actualRoot);
         //put them in graphs to assert links
         assertEquals(new GraphCallsRegister(root), new GraphCallsRegister(actualRoot));
      }
   }
}
