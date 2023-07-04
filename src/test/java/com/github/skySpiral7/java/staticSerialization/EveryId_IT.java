package com.github.skySpiral7.java.staticSerialization;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.testClasses.GraphCallsBoiler;
import com.github.skySpiral7.java.staticSerialization.testClasses.GraphCallsReflection;
import com.github.skySpiral7.java.staticSerialization.testClasses.GraphCallsRegister;
import com.github.skySpiral7.java.staticSerialization.testClasses.GraphUnregistered;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;

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
   public void noId() throws Exception
   {
      final File tempFile = File.createTempFile("EveryId_IT.TempFile.noId.", ".txt");
      tempFile.deleteOnExit();
      final String data = "hi";

      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      baos.write(new byte[]{'*', 0, 0, 0, 2});   //type (String) and UTF-8 length
      baos.write("hi".getBytes(StandardCharsets.UTF_8));   //value
      final byte[] expectedInFile = baos.toByteArray();

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();

      final byte[] actualInFile = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(new String(expectedInFile, StandardCharsets.UTF_8), new String(actualInFile, StandardCharsets.UTF_8));
      assertEquals(Arrays.toString(expectedInFile).replace(", ", ",\n"), Arrays.toString(actualInFile).replace(", ", ",\n"));

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertEquals(data, reader.readObject(String.class));
      reader.close();
   }

   /**
    * same object twice: uses id to save space even though there's no circle
    */
   @Test
   public void sameObject() throws IOException
   {
      final File tempFile = File.createTempFile("EveryId_IT.TempFile.sameObject.", ".txt");
      tempFile.deleteOnExit();
      final BigInteger same = new BigInteger("10");
      final BigInteger bigOne = new BigInteger("1");

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(same);
      writer.writeObject(bigOne);
      writer.writeObject(same);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
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
   public void sameObjectInArray() throws IOException
   {
      final File tempFile = File.createTempFile("EveryId_IT.TempFile.sameObjectInArray.", ".txt");
      tempFile.deleteOnExit();
      final BigInteger same = new BigInteger("10");
      final BigInteger bigOne = new BigInteger("1");
      final Object[] data = {same, bigOne, same};
      assertSame(data[0], data[2]);

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      final Object[] actual = reader.readObject(Object[].class);
      assertArrayEquals(data, actual);
      assertSame(actual[0], actual[2]);
      assertNotSame(data[0], actual[2]);
      reader.close();
   }

   /**
    * no ids for Number[]: as a happy path for arrays
    */
   @Test
   public void numberArrayNoId() throws IOException
   {
      final File tempFile = File.createTempFile("EveryId_IT.TempFile.numberArrayNoId.", ".txt");
      tempFile.deleteOnExit();
      final Object[] data = new Number[]{2};

      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      baos.write(new byte[]{'[', 1});   //data array indicator and dimensions
      baos.write("java.lang.Number;".getBytes(StandardCharsets.UTF_8));  //component
      baos.write(new byte[]{0, 0, 0, 1});   //array length
      baos.write(new byte[]{'@'});   //element type (Integer)
      baos.write(new byte[]{0, 0, 0, 2});   //element
      final byte[] expectedInFile = baos.toByteArray();

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();

      final byte[] actualInFile = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(new String(expectedInFile, StandardCharsets.UTF_8), new String(actualInFile, StandardCharsets.UTF_8));
      assertEquals(Arrays.toString(expectedInFile).replace(", ", ",\n"), Arrays.toString(actualInFile).replace(", ", ",\n"));

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertEquals(Arrays.deepToString(data), Arrays.deepToString(reader.readObject(Object[][][].class)));
      reader.close();
   }

   /**
    * no ids for Object[]: to show index is better
    */
   @Test
   public void objectArrayNoId() throws IOException
   {
      final File tempFile = File.createTempFile("EveryId_IT.TempFile.objectArrayNoId.", ".txt");
      tempFile.deleteOnExit();
      final Object[] data = {2};

      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      baos.write(new byte[]{'[', 1});   //data array indicator and dimensions
      baos.write("java.lang.Object;".getBytes(StandardCharsets.UTF_8));  //component
      baos.write(new byte[]{0, 0, 0, 1});   //array length
      baos.write(new byte[]{'@'});   //element type (Integer)
      baos.write(new byte[]{0, 0, 0, 2});   //element
      final byte[] expectedInFile = baos.toByteArray();

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();

      final byte[] actualInFile = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(new String(expectedInFile, StandardCharsets.UTF_8), new String(actualInFile, StandardCharsets.UTF_8));
      assertEquals(Arrays.toString(expectedInFile).replace(", ", ",\n"), Arrays.toString(actualInFile).replace(", ", ",\n"));

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertEquals(Arrays.deepToString(data), Arrays.deepToString(reader.readObject(Object[][][].class)));
      reader.close();
   }

   /**
    * <p>Object[] with ids: to auto-handle circles</p>
    * <p>This test case exists to validate an edge case since Object[] is the only array that can contain itself</p>
    */
   @Test
   public void objectArrayOfSelf() throws IOException
   {
      final File tempFile = File.createTempFile("EveryId_IT.TempFile.objectArrayOfSelf.", ".txt");
      tempFile.deleteOnExit();
      final Object[] data = {1, 0};
      data[1] = data;

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
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
   public void handlesDeepIds() throws Exception
   {
      final File tempFile = File.createTempFile("EveryId_IT.TempFile.handlesDeepIds.", ".txt");
      tempFile.deleteOnExit();
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

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      //write both the graph and root to show that self-referencing is handled inside an object and as the root object being written
      writer.writeObject(graph);
      writer.writeObject(root);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
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
   public void rootNode() throws Exception
   {
      final File tempFile = File.createTempFile("EveryId_IT.TempFile.rootNode.", ".txt");
      tempFile.deleteOnExit();
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

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      //shows that self-referencing is handled as the root object being written
      writer.writeObject(root);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
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
   public void boilerplate() throws Exception
   {
      final File tempFile = File.createTempFile("EveryId_IT.TempFile.boilerplate.", ".txt");
      tempFile.deleteOnExit();
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

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      //shows that self-referencing is handled as the root object being written
      writer.writeObject(root);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
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
   public void myClassFailedToRegister() throws Exception
   {
      final File tempFile = File.createTempFile("EveryId_IT.TempFile.myClassFailedToRegister.", ".txt");
      tempFile.deleteOnExit();
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

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      //shows that self-referencing is handled as the root object being written
      writer.writeObject(root);
      writer.close();

      try (ObjectStreamReader reader = new ObjectStreamReader(tempFile))
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
   public void deepFailedToRegister() throws Exception
   {
      final File tempFile = File.createTempFile("EveryId_IT.TempFile.deepFailedToRegister.", ".txt");
      tempFile.deleteOnExit();
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

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(graph);
      writer.close();

      try (ObjectStreamReader reader = new ObjectStreamReader(tempFile))
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
   public void reflectionDeep() throws Exception
   {
      final File tempFile = File.createTempFile("EveryId_IT.TempFile.reflectionDeep.", ".txt");
      tempFile.deleteOnExit();
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

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(graph);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      final GraphCallsReflection actualGraph = reader.readObject(GraphCallsReflection.class);
      reader.close();
      assertNotSame(graph, actualGraph);
      assertEquals(graph, actualGraph);
   }

   /**
    * Node reflection: have reflection method call registerObject for you
    */
   @Test
   public void reflectionForMe() throws Exception
   {
      final File tempFile = File.createTempFile("EveryId_IT.TempFile.reflectionForMe.", ".txt");
      tempFile.deleteOnExit();
      final GraphCallsReflection.Node root = new GraphCallsReflection.Node("Alice");
      {
         final GraphCallsReflection.Node bob = new GraphCallsReflection.Node("Bob");
         final GraphCallsReflection.Node clark = new GraphCallsReflection.Node("Clark");

         root.next = bob;
         bob.next = clark;
         clark.next = bob;
         //a -> b <-> c
      }

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(root);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      final GraphCallsReflection.Node actualRoot = reader.readObject();
      reader.close();
      assertNotSame(root, actualRoot);
      //put them in graphs to assert links
      assertEquals(new GraphCallsReflection(root), new GraphCallsReflection(actualRoot));
   }
}
