package com.github.skySpiral7.java.staticSerialization;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StringSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import com.github.skySpiral7.java.staticSerialization.testClasses.GraphCallsBoiler;
import com.github.skySpiral7.java.staticSerialization.testClasses.GraphCallsReflection;
import com.github.skySpiral7.java.staticSerialization.testClasses.GraphCallsRegister;
import com.github.skySpiral7.java.staticSerialization.testClasses.GraphUnregistered;
import org.junit.jupiter.api.Test;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * <h2>every id related scenario</h2>
 * <ul>
 * <li>no ids needed or present: all normal tests eg a String</li>
 * <li>same object twice: uses id to save space even though there's no circle</li>
 * <li>no ids for Number[]: as a happy path for arrays</li>
 * <li>no ids for Object[]: to show index is better</li>
 * <li>Object[] with ids: to auto-handle circles</li>
 * <li>RootedGraph: ids are handled deeply</li>
 * <li>Node: ids are handled for the root object</li>
 * <li>readFromStream calling StaticSerializable.readFromStream: to show boilerplate works</li>
 * <li>readFromStream calling registerObject: happy path for expected implementation</li>
 * <li>catch Node: readFromStream failing to call registerObject: to make sure user failure message is useful for you class</li>
 * <li>catch RootedGraph: readFromStream failing to call registerObject: to make sure user failure message is useful for deep</li>
 * <li>RootedGraph reflection: easy enough</li>
 * <li>Node reflection: have reflection method call registerObject for you</li>
 * </ul>
 *
 * <h2>do not do</h2>
 * <ul>
 * <li>calling readFromStream directly when null/id: pretty sure there's no error message I can give</li>
 * <li>calling RootedGraph.readFromStream would work: no possible error message</li>
 * <li>calling Node.readFromStream won't work: might be able to do error message</li>
 * </ul>
 *
 * <h2>test classes</h2>
 * <ul>
 * <li>used for most: calls registerObject</li>
 * <li>calls StaticSerializable.readFromStream</li>
 * <li>does not call registerObject</li>
 * <li>reflection</li>
 * </ul>
 */
public class EveryId_IT
{
   /**
    * no ids needed or present: all normal tests eg a String
    */
   @Test
   public void noId()
   {
      final String data = "hi";

      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append((byte) '*');   //type (String)
      expectedBuilder.append("hi");   //value
      expectedBuilder.append(StringSerializableStrategy.TERMINATOR);
      final byte[] expectedInFile = expectedBuilder.getAllBytes();

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      final byte[] actualInFile = mockFile.getAllBytes();
      assertEquals(new String(expectedInFile, StandardCharsets.UTF_8), new String(actualInFile, StandardCharsets.UTF_8));
      assertEquals(Arrays.toString(expectedInFile).replace(", ", ",\n"), Arrays.toString(actualInFile).replace(", ", ",\n"));

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(actualInFile));
      assertEquals(data, reader.readObject(String.class));
      reader.close();
   }

   /**
    * same object twice: uses id to save space even though there's no circle
    */
   @Test
   public void sameObject()
   {
      final BigInteger same = new BigInteger("10");
      final BigInteger bigOne = new BigInteger("1");

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(same);
      writer.writeObject(bigOne);
      writer.writeObject(same);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      final BigInteger actualSame = reader.readObject(BigInteger.class);
      reader.readObject(BigInteger.class);  //bigOne
      assertSame(actualSame, reader.readObject(BigInteger.class));
      assertNotSame(same, actualSame);
      reader.close();
   }

   /**
    * same object twice: uses id to save space even though there's no circle
    */
   @Test
   public void sameObjectInArray()
   {
      final BigInteger same = new BigInteger("10");
      final BigInteger bigOne = new BigInteger("1");
      final Object[] data = {same, bigOne, same};
      assertSame(data[0], data[2]);

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      final Object[] actual = reader.readObject(Object[].class);
      assertThat(actual, is(data));
      assertSame(actual[0], actual[2]);
      assertNotSame(data[0], actual[2]);
      reader.close();
   }

   /**
    * no ids for Number[]: as a happy path for arrays
    */
   @Test
   public void numberArrayNoId()
   {
      final Object[] data = new Number[]{2};

      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{'[', 1});   //data array indicator and dimensions
      expectedBuilder.append("java.lang.Number");  //component
      expectedBuilder.append(StringSerializableStrategy.TERMINATOR);
      expectedBuilder.append(new byte[]{0, 0, 0, 1});   //array length
      expectedBuilder.append(new byte[]{'@'});   //element type (Integer)
      expectedBuilder.append(new byte[]{0, 0, 0, 2});   //element
      final byte[] expectedInFile = expectedBuilder.getAllBytes();

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      final byte[] actualInFile = mockFile.getAllBytes();
      assertEquals(new String(expectedInFile, StandardCharsets.UTF_8), new String(actualInFile, StandardCharsets.UTF_8));
      assertEquals(Arrays.toString(expectedInFile).replace(", ", ",\n"), Arrays.toString(actualInFile).replace(", ", ",\n"));

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(actualInFile));
      assertEquals(Arrays.deepToString(data), Arrays.deepToString(reader.readObject(Object[][][].class)));
      reader.close();
   }

   /**
    * no ids for Object[]: to show index is better
    */
   @Test
   public void objectArrayNoId()
   {
      final Object[] data = {2};

      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{'[', 1});   //data array indicator and dimensions
      expectedBuilder.append("java.lang.Object");  //component
      expectedBuilder.append(StringSerializableStrategy.TERMINATOR);
      expectedBuilder.append(new byte[]{0, 0, 0, 1});   //array length
      expectedBuilder.append(new byte[]{'@'});   //element type (Integer)
      expectedBuilder.append(new byte[]{0, 0, 0, 2});   //element
      final byte[] expectedInFile = expectedBuilder.getAllBytes();

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      final byte[] actualInFile = mockFile.getAllBytes();
      assertEquals(new String(expectedInFile, StandardCharsets.UTF_8), new String(actualInFile, StandardCharsets.UTF_8));
      assertEquals(Arrays.toString(expectedInFile).replace(", ", ",\n"), Arrays.toString(actualInFile).replace(", ", ",\n"));

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(actualInFile));
      assertEquals(Arrays.deepToString(data), Arrays.deepToString(reader.readObject(Object[][][].class)));
      reader.close();
   }

   /**
    * <p>Object[] with ids: to auto-handle circles</p>
    * <p>This test case exists to validate an edge case since Object[] is the only array that can contain itself</p>
    */
   @Test
   public void objectArrayOfSelf()
   {
      final Object[] data = {1, 0};
      data[1] = data;

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      final Object[] actual = reader.readObject(Object[].class);
      reader.close();
      assertEquals(Arrays.deepToString(data), Arrays.deepToString(actual));
      assertSame(actual, actual[1]);
   }

   /**
    * <p>RootedGraph: ids are handled deeply</p>
    * <p>readFromStream calling registerObject: happy path for expected implementation</p>
    */
   @Test
   public void handlesDeepIds()
   {
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

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      //write both the graph and root to show that self-referencing is handled inside an object and as the root object being written
      writer.writeObject(graph);
      writer.writeObject(root);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      final GraphCallsRegister actualGraph = reader.readObject(GraphCallsRegister.class);
      final GraphCallsRegister.Node actualRoot = reader.readObject(GraphCallsRegister.Node.class);
      reader.close();
      assertNotSame(graph, actualGraph);
      assertEquals(graph, actualGraph);
      assertSame(actualGraph.getRoot(), actualRoot);
   }

   /**
    * <p>Node: ids are handled for the root object</p>
    * <p>readFromStream calling registerObject: happy path for expected implementation</p>
    */
   @Test
   public void rootNode()
   {
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

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      //shows that self-referencing is handled as the root object being written
      writer.writeObject(root);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      final GraphCallsRegister.Node actualRoot = reader.readObject();
      reader.close();
      assertNotSame(root, actualRoot);
      //put them in graphs to assert links
      assertEquals(new GraphCallsRegister(root), new GraphCallsRegister(actualRoot));
   }

   /**
    * readFromStream calling StaticSerializable.readFromStream: to show boilerplate works
    */
   @Test
   public void boilerplate()
   {
      final GraphCallsBoiler.Node root = new GraphCallsBoiler.Node("Alice");
      {
         final GraphCallsBoiler.Node bob = new GraphCallsBoiler.Node("Bob");
         final GraphCallsBoiler.Node clark = new GraphCallsBoiler.Node("Clark");

         root.links.add(bob);
         bob.links.add(clark);
         clark.links.add(bob);
         clark.links.add(clark);
         //a -> b <-> c -> c
      }

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      //shows that self-referencing is handled as the root object being written
      writer.writeObject(root);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      final GraphCallsBoiler.Node actualRoot = reader.readObject();
      reader.close();
      assertNotSame(root, actualRoot);
      //put them in graphs to assert links
      assertEquals(new GraphCallsBoiler(root), new GraphCallsBoiler(actualRoot));
   }

   /**
    * catch Node: readFromStream failing to call registerObject: to make sure user failure message is useful for your class
    */
   @Test
   public void myClassFailedToRegister()
   {
      final GraphUnregistered.Node root = new GraphUnregistered.Node("Alice");
      {
         final GraphUnregistered.Node bob = new GraphUnregistered.Node("Bob");
         final GraphUnregistered.Node clark = new GraphUnregistered.Node("Clark");

         root.links.add(bob);
         bob.links.add(clark);
         clark.links.add(bob);
         clark.links.add(clark);
         //a -> b <-> c -> c
      }

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      //shows that self-referencing is handled as the root object being written
      writer.writeObject(root);
      writer.close();

      try (ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes())))
      {
         reader.readObject();
         fail("Should've thrown");
      }
      catch (Exception actual)
      {
         //actual.getCause().getCause().getCause().getCause().getCause().getCause()
         //TODO: see if I can get this less buried...
         //TODO: can this message be more specific?
         final Throwable actualRootCause = getRootCause(actual);
         assertEquals(StreamCorruptedException.class, actualRootCause.getClass());
         assertEquals("id not found", actualRootCause.getMessage());
      }
   }

   private Throwable getRootCause(Throwable result)
   {
      while (null != result.getCause())
      {
         result = result.getCause();
      }
      return result;
   }

   /**
    * catch RootedGraph: readFromStream failing to call registerObject: to make sure user failure message is useful for deep
    */
   @Test
   public void deepFailedToRegister()
   {
      final GraphUnregistered graph;
      final GraphUnregistered.Node root = new GraphUnregistered.Node("Alice");
      {
         final GraphUnregistered.Node bob = new GraphUnregistered.Node("Bob");
         final GraphUnregistered.Node clark = new GraphUnregistered.Node("Clark");

         root.links.add(bob);
         bob.links.add(clark);
         clark.links.add(bob);
         clark.links.add(clark);
         //a -> b <-> c -> c

         graph = new GraphUnregistered(root);
      }

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(graph);
      writer.close();

      try (ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes())))
      {
         reader.readObject();
         fail("Should've thrown");
      }
      catch (Exception actual)
      {
         final Throwable actualRootCause = getRootCause(actual);
         assertEquals(StreamCorruptedException.class, actualRootCause.getClass());
         assertEquals("id not found", actualRootCause.getMessage());
      }
   }

   /**
    * RootedGraph reflection: easy enough
    */
   @Test
   public void reflectionDeep()
   {
      final GraphCallsReflection graph;
      final GraphCallsReflection.Node root = new GraphCallsReflection.Node("Alice");
      {
         final GraphCallsReflection.Node bob = new GraphCallsReflection.Node("Bob");
         final GraphCallsReflection.Node clark = new GraphCallsReflection.Node("Clark");

         root.next = bob;
         bob.next = clark;
         clark.next = bob;
         //a -> b <-> c

         graph = new GraphCallsReflection(root);
      }

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(graph);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      final GraphCallsReflection actualGraph = reader.readObject(GraphCallsReflection.class);
      reader.close();
      assertNotSame(graph, actualGraph);
      assertEquals(graph, actualGraph);
   }

   /**
    * Node reflection: have reflection method call registerObject for you
    */
   @Test
   public void reflectionForMe()
   {
      final GraphCallsReflection.Node root = new GraphCallsReflection.Node("Alice");
      {
         final GraphCallsReflection.Node bob = new GraphCallsReflection.Node("Bob");
         final GraphCallsReflection.Node clark = new GraphCallsReflection.Node("Clark");

         root.next = bob;
         bob.next = clark;
         clark.next = bob;
         //a -> b <-> c
      }

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(root);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      final GraphCallsReflection.Node actualRoot = reader.readObject();
      reader.close();
      assertNotSame(root, actualRoot);
      //put them in graphs to assert links
      assertEquals(new GraphCallsReflection(root), new GraphCallsReflection(actualRoot));
   }
}
