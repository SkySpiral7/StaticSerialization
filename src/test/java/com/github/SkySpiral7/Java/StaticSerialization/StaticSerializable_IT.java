package com.github.SkySpiral7.Java.StaticSerialization;

import java.io.File;
import java.math.BigInteger;
import java.math.RoundingMode;

import com.github.SkySpiral7.Java.StaticSerialization.testClasses.RootedGraph;
import com.github.SkySpiral7.Java.StaticSerialization.testClasses.RootedGraph.Node;
import com.github.SkySpiral7.Java.StaticSerialization.testClasses.SimpleHappy;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

public class StaticSerializable_IT
{
   @Test
   public void value_null() throws Exception
   {
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.value_null.", ".txt");
      tempFile.deleteOnExit();

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(null);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertNull(reader.readObject(byte.class));
      reader.close();
   }

   @Test
   public void primitive_byte() throws Exception
   {
      //This test case exists because primitives have a special format
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.primitive_byte.", ".txt");
      tempFile.deleteOnExit();
      final byte data = (byte) 2;

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertEquals(Byte.valueOf(data), reader.readObject(byte.class));
      reader.close();
   }

   @Test
   public void primitive_short() throws Exception
   {
      //This test case exists because primitives have a special format
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.primitive_short.", ".txt");
      tempFile.deleteOnExit();
      final short data = (short) 2;

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertEquals(Short.valueOf(data), reader.readObject(short.class));
      reader.close();
   }

   @Test
   public void primitive_int() throws Exception
   {
      //This test case exists because primitives have a special format
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.primitive_int.", ".txt");
      tempFile.deleteOnExit();
      final int data = 2;

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertEquals(Integer.valueOf(data), reader.readObject(int.class));
      reader.close();
   }

   @Test
   public void primitive_long() throws Exception
   {
      //This test case exists because primitives have a special format
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.primitive_long.", ".txt");
      tempFile.deleteOnExit();
      final long data = 2L;

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertEquals(Long.valueOf(data), reader.readObject(long.class));
      reader.close();
   }

   @Test
   public void primitive_float() throws Exception
   {
      //This test case exists because primitives have a special format
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.primitive_float.", ".txt");
      tempFile.deleteOnExit();
      final float data = 2.0F;

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertEquals(Float.valueOf(data), reader.readObject(float.class));
      reader.close();
   }

   @Test
   public void primitive_double() throws Exception
   {
      //This test case exists because primitives have a special format
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.primitive_double.", ".txt");
      tempFile.deleteOnExit();
      final double data = 2.0D;

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertEquals(Double.valueOf(data), reader.readObject(double.class));
      reader.close();
   }

   @Test
   public void primitive_boolean() throws Exception
   {
      //This test case exists because primitives have a special format
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.primitive_boolean.", ".txt");
      tempFile.deleteOnExit();

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(true);
      writer.writeObject(false);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertTrue(reader.readObject(boolean.class));
      assertFalse(reader.readObject(Boolean.class));
      reader.close();
   }

   @Test
   public void primitive_char() throws Exception
   {
      //This test case exists because primitives have a special format
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.primitive_char.", ".txt");
      tempFile.deleteOnExit();
      final byte data = (byte) 2;

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertEquals(Byte.valueOf(data), reader.readObject(byte.class));
      reader.close();
   }

   @Test
   public void string() throws Exception
   {
      //This test case exists because Strings have a special format
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.string.", ".txt");
      tempFile.deleteOnExit();
      final String data = "\u0000\u221E > \uD83D\uDE22";  //control (null), BMP (infinity), ascii, non-BMP (Crying Face)

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertEquals(data, reader.readObject(String.class));
      reader.close();
   }

   @Test
   public void custom() throws Exception
   {
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.custom.", ".txt");
      tempFile.deleteOnExit();
      final SimpleHappy data = new SimpleHappy(4);

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);

      final SimpleHappy actual = reader.readObject(SimpleHappy.class);
      assertNotSame(data, actual);
      assertEquals(data, actual);
      reader.close();
   }

   @Test
   public void custom_allowsDirectCalling() throws Exception
   {
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.custom_allowsDirectCalling.", ".txt");
      tempFile.deleteOnExit();
      final SimpleHappy data = new SimpleHappy(4);

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      data.writeToStream(writer);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);

      final SimpleHappy actual = SimpleHappy.readFromStream(reader);
      assertNotSame(data, actual);
      assertEquals(data, actual);
      reader.close();
   }

   @Test
   public void normalEnum() throws Exception
   {
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.normalEnum.", ".txt");
      tempFile.deleteOnExit();

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(RoundingMode.HALF_DOWN);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);

      assertSame(RoundingMode.HALF_DOWN, reader.readObject(RoundingMode.class));
      reader.close();
   }

   private static enum CustomEnum implements StaticSerializable
   {
      One, Two;

      public static CustomEnum readFromStream(final ObjectStreamReader reader)
      {
         final String name = reader.readObject(String.class);
         return CustomEnum.valueOf(name);
      }

      @Override
      public void writeToStream(final ObjectStreamWriter writer)
      {
         writer.writeObject(this.name());
      }
   }

   @Test
   public void customEnum() throws Exception
   {
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.customEnum.", ".txt");
      tempFile.deleteOnExit();

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(CustomEnum.One);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);

      assertSame(CustomEnum.One, reader.readObject(CustomEnum.class));
      reader.close();
   }

   @Test
   public void rootedGraph() throws Exception
   {
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.rootedGraph.", ".txt");
      tempFile.deleteOnExit();
      final RootedGraph graph;
      final Node root = new Node("Alice");
      {
         final Node bob = new Node("Bob");
         final Node clark = new Node("Clark");

         root.links.add(bob);
         bob.links.add(clark);
         clark.links.add(bob);
         clark.links.add(clark);
         //a -> b <-> c -> c

         graph = new RootedGraph(root);
      }

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      //write both the graph and root to show that self-referencing is handled inside an object and as the root object being written
      writer.writeObject(graph);
      writer.writeObject(root);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      final RootedGraph actualGraph = reader.readObject(RootedGraph.class);
      final RootedGraph.Node actualRoot = reader.readObject(RootedGraph.Node.class);
      reader.close();
      assertNotSame(graph, actualGraph);
      assertEquals(graph, actualGraph);
      assertSame(actualGraph.getRoot(), actualRoot);
   }

   @Test
   public void rootNode() throws Exception
   {
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.rootNode.", ".txt");
      tempFile.deleteOnExit();
      final Node root = new Node("Alice");
      {
         final Node bob = new Node("Bob");
         final Node clark = new Node("Clark");

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
      final Node actualRoot = reader.readObject();
      reader.close();
      assertNotSame(root, actualRoot);
      assertEquals(root, actualRoot);
   }

   @Test
   public void rootNode_allowsDirectCalling() throws Exception
   {
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.rootNode_allowsDirectCalling.", ".txt");
      tempFile.deleteOnExit();
      final RootedGraph.Node root = new RootedGraph.Node("Alice");
      {
         final RootedGraph.Node bob = new RootedGraph.Node("Bob");
         final RootedGraph.Node clark = new RootedGraph.Node("Clark");

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
      final RootedGraph.Node actualRoot = RootedGraph.Node.readFromStream(reader);
      reader.close();
      assertNotSame(root, actualRoot);
      assertEquals(root, actualRoot);
   }

   private static final class ReflectiveClass implements StaticSerializable
   {
      private int field = 0xdead_beef;

      public static ReflectiveClass readFromStream(final ObjectStreamReader reader)
      {
         final ReflectiveClass result = new ReflectiveClass();
         reader.readFieldsReflectively(result);
         return result;
      }

      @Override
      public void writeToStream(final ObjectStreamWriter writer)
      {
         writer.writeFieldsReflectively(this);
      }
   }

   @Test
   public void reflection() throws Exception
   {
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.reflection.", ".txt");
      tempFile.deleteOnExit();
      final ReflectiveClass data = new ReflectiveClass();
      data.field = 0x0afe_babe;

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);

      final ReflectiveClass actual = reader.readObject(ReflectiveClass.class);
      assertNotSame(data, actual);
      assertEquals(data.field, actual.field);
      reader.close();
   }

   @Test
   public void serializable() throws Exception
   {
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.serializable.", ".txt");
      tempFile.deleteOnExit();
      final BigInteger data = BigInteger.TEN;

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);

      final BigInteger actual = (BigInteger) reader.readObject();
      assertNotSame(data, actual);  //TEN is not a singleton and BigInteger won't readResolve it to be the same
      assertEquals(data, actual);
      reader.close();
   }
}
