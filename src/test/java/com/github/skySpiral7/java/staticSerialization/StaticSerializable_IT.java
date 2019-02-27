package com.github.skySpiral7.java.staticSerialization;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import com.github.skySpiral7.java.staticSerialization.testClasses.RootedGraph;
import com.github.skySpiral7.java.staticSerialization.testClasses.RootedGraph.Node;
import com.github.skySpiral7.java.staticSerialization.testClasses.SimpleHappy;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Test;

import static org.junit.Assert.assertArrayEquals;
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
      writer.writeObject(null);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertNull(reader.readObject(String.class));
      assertNull(reader.readObject(byte.class));
      reader.close();
   }

   @Test
   public void primitive_byte() throws Exception
   {
      //This test case exists because primitives have a special format.
      //TODO: include boxes and arrays in these tests?
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.primitive_byte.", ".txt");
      tempFile.deleteOnExit();
      final byte data = 2;

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
      //This test case exists because primitives have a special format.
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
      //This test case exists because primitives have a special format.
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
      //This test case exists because primitives have a special format.
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
      //This test case exists because primitives have a special format.
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
      //This test case exists because primitives have a special format.
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
      //This test case exists because primitives have a special format.
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
      //This test case exists because primitives have a special format.
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.primitive_char.", ".txt");
      tempFile.deleteOnExit();
      final char data = 'f';

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertEquals(Character.valueOf(data), reader.readObject(char.class));
      reader.close();
   }

   @Test
   public void string() throws Exception
   {
      //This test case exists because Strings have a special format.
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.string.", ".txt");
      tempFile.deleteOnExit();
      final String data = "\u0000∞ > 😢";  //control (null), BMP (infinity), ascii, non-BMP (Crying Face)

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertEquals(data, reader.readObject(String.class));
      reader.close();
   }

   @Test
   public void objectArray() throws IOException
   {
      //This test case exists because Arrays have a special format.
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.objectArray.", ".txt");
      tempFile.deleteOnExit();
      final Object[] data = {1, "joe"};

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertArrayEquals(data, reader.readObject(Object[].class));
      reader.close();
   }

   @Test
   public void boxArray() throws IOException
   {
      //This test case exists because Arrays have a special format.
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.boxArray.", ".txt");
      tempFile.deleteOnExit();
      final Integer[] data = {1, 5};

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertArrayEquals(data, reader.readObject(Integer[].class));
      reader.close();
   }

   @Test
   public void primitiveArray() throws IOException
   {
      //This test case exists because Arrays have a special format.
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.primitiveArray.", ".txt");
      tempFile.deleteOnExit();
      final int[] data = {1, 5};

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertArrayEquals(data, reader.readObject(int[].class));
      reader.close();
   }

   @Test
   public void array2D() throws IOException
   {
      //This test case exists because Arrays have a special format.
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.array2D.", ".txt");
      tempFile.deleteOnExit();
      final Byte[][] data = {{1}, null};

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertArrayEquals(data, reader.readObject(Byte[][].class));
      reader.close();
   }

   @Test
   public void arrayStressTest() throws IOException
   {
      //This test case exists to prove that the complexities of arrays are all handled correctly.
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.arrayStressTest.", ".txt");
      tempFile.deleteOnExit();
      final Object[][][] data = new Object[3][][];
      data[0] = new CharSequence[][]{new String[]{"hi"}};
      data[1] = new Number[][]{new Integer[]{1, 2}, new Long[]{4L, 5L}};
      data[2] = new Object[1][1];
      data[2][0][0] = new Object[]{null, "joe", new int[]{6}, data};
      //TODO: make ids non-random int then rebase this. question: should all arrays have ids? is both options possible?
      //idea: keep list of objects so far when writing, new type of id, id is list index, now full auto no waste!

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();

      /*
Object graph (using non compressed names):
[3java.lang.Object;<id 1>3
   java.lang.CharSequence;1
      java.lang.String;1
         ? 0002 hi
   java.lang.Number;2
      java.lang.Integer;2
         ? 0001
         ? 0002
      java.lang.Long;2
         ? 00000004
         ? 00000005
   ?<id>1
      ?<id>1
         [1java.lang.Object;<id>3
            ;
            java.lang.String; 0003 joe
            [1int;1
               0006
            [3java.lang.Object;<id 1>
       */
      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      baos.write(new byte[]{'[', 3});   //data array indicator and dimensions
      baos.write("java.lang.Object;".getBytes(StandardCharsets.UTF_8));   //data component
      baos.write(new byte[]{0, 0, 0, 3});   //data length
      baos.write("java.lang.CharSequence;".getBytes(StandardCharsets.UTF_8));   //data[0] component
      baos.write(new byte[]{0, 0, 0, 1});   //data[0] length
      baos.write(new byte[]{'*'});   //data[0][0] component (String)
      baos.write(new byte[]{0, 0, 0, 1});   //data[0][0] length
      baos.write(new byte[]{'?', 0, 0, 0, 2});   //data[0][0][0] inherited type and UTF-8 length
      baos.write("hi".getBytes(StandardCharsets.UTF_8));   //data[0][0][0] value
      baos.write("java.lang.Number;".getBytes(StandardCharsets.UTF_8));   //data[1] component
      baos.write(new byte[]{0, 0, 0, 2});   //data[1] length
      baos.write(new byte[]{'@'});   //data[1][0] array component (Integer)
      baos.write(new byte[]{0, 0, 0, 2});   //data[1][0] length
      baos.write(new byte[]{'?', 0, 0, 0, 1});   //data[1][0][0] inherited type and value
      baos.write(new byte[]{'?', 0, 0, 0, 2});   //data[1][0][1] inherited type and value
      baos.write(new byte[]{'#'});   //data[1][1] array component (Long)
      baos.write(new byte[]{0, 0, 0, 2});   //data[1][1] length
      baos.write(new byte[]{'?', 0, 0, 0, 0, 0, 0, 0, 4});   //data[1][1][0] inherited type and value
      baos.write(new byte[]{'?', 0, 0, 0, 0, 0, 0, 0, 5});   //data[1][1][1] inherited type and value
      baos.write(new byte[]{'?'});   //data[2] inherited type
      baos.write(new byte[]{0, 0, 0, 1});   //data[2] length
      baos.write(new byte[]{'?'});   //data[2][0] inherited type
      baos.write(new byte[]{0, 0, 0, 1});   //data[2][0] length
      baos.write(new byte[]{'[', 1});   //data[2][0][0] array indicator and dimensions
      baos.write("java.lang.Object;".getBytes(StandardCharsets.UTF_8));   //data[2][0][0] component
      baos.write(new byte[]{0, 0, 0, 3});   //data[2][0][0] length
      baos.write(new byte[]{';'});   //data[2][0][0][0]=null
      baos.write(new byte[]{'*', 0, 0, 0, 3});   //data[2][0][0][1] type (String) and UTF-8 length
      baos.write("joe".getBytes(StandardCharsets.UTF_8));   //data[2][0][0][1] value
      baos.write(new byte[]{']', 1, '@'});   //data[2][0][0][2] array indicator, dimensions, component (int)
      baos.write(new byte[]{0, 0, 0, 1});   //data[2][0][0][2] length
      baos.write(new byte[]{0, 0, 0, 6});   //data[2][0][0][2] value (no header)

      final byte[] expectedInFile = baos.toByteArray();
      final byte[] actualInFile = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(new String(expectedInFile, StandardCharsets.UTF_8), new String(actualInFile, StandardCharsets.UTF_8));
      assertEquals(Arrays.toString(expectedInFile).replace(", ", ",\n"), Arrays.toString(actualInFile).replace(", ", ",\n"));

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertEquals(Arrays.deepToString(data), Arrays.deepToString(reader.readObject(Object[][][].class)));
      reader.close();
   }

   @Test
   public void objectArrayOfSupported() throws IOException
   {
      //This test case exists to validate an edge case since Object.class isn't supported.
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.objectArrayOfSupported.", ".txt");
      tempFile.deleteOnExit();
      final Object[] data = {1};

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertEquals(Arrays.deepToString(data), Arrays.deepToString(reader.readObject(Object[].class)));
      reader.close();
   }

   @Test
   public void primitiveBooleanArray() throws IOException
   {
      //This test case exists because boolean[] is an edge case.
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.primitiveBooleanArray.", ".txt");
      tempFile.deleteOnExit();
      final boolean[] data = new boolean[]{true, false};

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertArrayEquals(data, reader.readObject(boolean[].class));
      reader.close();
   }

   @Test
   public void boxBooleanArray() throws IOException
   {
      //This test case exists because Boolean[] is an edge case.
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.boxBooleanArray.", ".txt");
      tempFile.deleteOnExit();
      final Boolean[] data = new Boolean[]{true, false};

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertArrayEquals(data, reader.readObject(Boolean[].class));
      reader.close();
   }

   @Test
   public void emptyArray() throws IOException
   {
      //This test case exists because an empty array can be of a unsupported type.
      final File tempFile = File.createTempFile("StaticSerializable_IT.TempFile.emptyArray.", ".txt");
      tempFile.deleteOnExit();
      final Void[] data = new Void[0];

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertArrayEquals(data, reader.readObject(Void[].class));
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

   public static enum CustomEnum implements StaticSerializable
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

   public static final class ReflectiveClass implements StaticSerializable
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

      final BigInteger actual = reader.readObject();
      assertNotSame(data, actual);  //TEN is not a singleton and BigInteger won't readResolve it to be the same
      assertEquals(data, actual);
      reader.close();
   }
}
