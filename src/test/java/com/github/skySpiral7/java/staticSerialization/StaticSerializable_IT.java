package com.github.skySpiral7.java.staticSerialization;

import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import com.github.skySpiral7.java.staticSerialization.testClasses.ChildImmutable;
import com.github.skySpiral7.java.staticSerialization.testClasses.ChildMutable;
import com.github.skySpiral7.java.staticSerialization.testClasses.GraphCallsRegister;
import com.github.skySpiral7.java.staticSerialization.testClasses.SimpleHappy;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.Test;

import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class StaticSerializable_IT
{
   private static final Logger LOG = LogManager.getLogger();

   @Test
   public void value_null()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(null);
      writer.writeObject(null);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertNull(reader.readObject(String.class));
      assertNull(reader.readObject(byte.class));
      reader.close();
   }

   @Test
   public void primitive_byte()
   {
      //This test case exists because primitives have a special format.
      //TODO: include boxes and arrays in these tests?
      final byte data = 2;

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertEquals(Byte.valueOf(data), reader.readObject(byte.class));
      reader.close();
   }

   @Test
   public void primitive_short()
   {
      //This test case exists because primitives have a special format.
      final short data = (short) 2;

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertEquals(Short.valueOf(data), reader.readObject(short.class));
      reader.close();
   }

   @Test
   public void primitive_int()
   {
      //This test case exists because primitives have a special format.
      final int data = 2;

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertEquals(Integer.valueOf(data), reader.readObject(int.class));
      reader.close();
   }

   @Test
   public void primitive_long()
   {
      //This test case exists because primitives have a special format.
      final long data = 2L;

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertEquals(Long.valueOf(data), reader.readObject(long.class));
      reader.close();
   }

   @Test
   public void primitive_float()
   {
      //This test case exists because primitives have a special format.
      final float data = 2.0F;

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertEquals(Float.valueOf(data), reader.readObject(float.class));
      reader.close();
   }

   @Test
   public void primitive_double()
   {
      //This test case exists because primitives have a special format.
      final double data = 2.0D;

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertEquals(Double.valueOf(data), reader.readObject(double.class));
      reader.close();
   }

   @Test
   public void primitive_boolean()
   {
      //This test case exists because primitives have a special format.
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(true);
      writer.writeObject(false);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertTrue(reader.readObject(boolean.class));
      assertFalse(reader.readObject(Boolean.class));
      reader.close();
   }

   @Test
   public void primitive_char()
   {
      //This test case exists because primitives have a special format.
      final char data = 'f';

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertEquals(Character.valueOf(data), reader.readObject(char.class));
      reader.close();
   }

   @Test
   public void string()
   {
      //This test case exists because Strings have a special format.
      final String data = "\u0000∞ > 😢";  //control (null), BMP (infinity), ascii, non-BMP (Crying Face)

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertEquals(data, reader.readObject(String.class));
      reader.close();
   }

   @Test
   public void objectArray()
   {
      //This test case exists because Arrays have a special format.
      final Object[] data = {1, "joe"};

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertArrayEquals(data, reader.readObject(Object[].class));
      reader.close();
   }

   @Test
   public void sameObject()
   {
      //This test case exists because Arrays have a special format.
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
      final BigInteger actualOne = reader.readObject(BigInteger.class);
      assertSame(actualSame, reader.readObject(BigInteger.class));
      assertNotSame(same, actualSame);
      reader.close();
   }

   @Test
   public void sameObjectInArray()
   {
      //This test case exists because Arrays have a special format.
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
      assertArrayEquals(data, actual);
      assertSame(actual[0], actual[2]);
      assertNotSame(data[0], actual[2]);
      reader.close();
   }

   @Test
   public void boxArray()
   {
      //This test case exists because Arrays have a special format.
      final Integer[] data = {1, 5};

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertArrayEquals(data, reader.readObject(Integer[].class));
      reader.close();
   }

   @Test
   public void primitiveArray()
   {
      //This test case exists because Arrays have a special format.
      final int[] data = {1, 5};

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertArrayEquals(data, reader.readObject(int[].class));
      reader.close();
   }

   @Test
   public void array2D()
   {
      //This test case exists because Arrays have a special format.
      final Byte[][] data = {{1}, null};

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertArrayEquals(data, reader.readObject(Byte[][].class));
      reader.close();
   }

   @Test
   public void arrayStressTest()
   {
      //This test case exists to prove that the complexities of arrays are all handled correctly.
      final Object[][][] data = new Object[3][][];
      data[0] = new CharSequence[][]{new String[]{"hi"}};
      data[1] = new Number[][]{new Integer[]{1, 2}, new Long[]{4L, 5L}};
      data[2] = new Object[1][1];
      data[2][0][0] = new Object[]{null, "joe", new int[]{6}, data};

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      /*
Object graph (using non compressed names):
[3java.lang.Object;<id 0>3
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
   ?
      ?
         [1java.lang.Object;4
            ;
            java.lang.String; 0003 joe
            [1int;1
               0006
            \<id 0>
       */
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{'[', 3});   //data array indicator and dimensions
      expectedBuilder.append("java.lang.Object;");   //data component
      expectedBuilder.append(new byte[]{0, 0, 0, 3});   //data length
      expectedBuilder.append("java.lang.CharSequence;");   //data[0] component
      expectedBuilder.append(new byte[]{0, 0, 0, 1});   //data[0] length
      expectedBuilder.append(new byte[]{'*'});   //data[0][0] component (String)
      expectedBuilder.append(new byte[]{0, 0, 0, 1});   //data[0][0] length
      expectedBuilder.append(new byte[]{'?', 0, 0, 0, 2});   //data[0][0][0] inherited type and UTF-8 length
      expectedBuilder.append("hi");   //data[0][0][0] value
      expectedBuilder.append("java.lang.Number;");   //data[1] component
      expectedBuilder.append(new byte[]{0, 0, 0, 2});   //data[1] length
      expectedBuilder.append(new byte[]{'@'});   //data[1][0] array component (Integer)
      expectedBuilder.append(new byte[]{0, 0, 0, 2});   //data[1][0] length
      expectedBuilder.append(new byte[]{'?', 0, 0, 0, 1});   //data[1][0][0] inherited type and value
      expectedBuilder.append(new byte[]{'?', 0, 0, 0, 2});   //data[1][0][1] inherited type and value
      expectedBuilder.append(new byte[]{'#'});   //data[1][1] array component (Long)
      expectedBuilder.append(new byte[]{0, 0, 0, 2});   //data[1][1] length
      expectedBuilder.append(new byte[]{'?', 0, 0, 0, 0, 0, 0, 0, 4});   //data[1][1][0] inherited type and value
      expectedBuilder.append(new byte[]{'?', 0, 0, 0, 0, 0, 0, 0, 5});   //data[1][1][1] inherited type and value
      expectedBuilder.append(new byte[]{'?'});   //data[2] inherited type
      expectedBuilder.append(new byte[]{0, 0, 0, 1});   //data[2] length
      expectedBuilder.append(new byte[]{'?'});   //data[2][0] inherited type
      expectedBuilder.append(new byte[]{0, 0, 0, 1});   //data[2][0] length
      expectedBuilder.append(new byte[]{'[', 1});   //data[2][0][0] array indicator and dimensions
      expectedBuilder.append("java.lang.Object;");   //data[2][0][0] component
      expectedBuilder.append(new byte[]{0, 0, 0, 4});   //data[2][0][0] length
      expectedBuilder.append(new byte[]{';'});   //data[2][0][0][0]=null
      expectedBuilder.append(new byte[]{'*', 0, 0, 0, 3});   //data[2][0][0][1] type (String) and UTF-8 length
      expectedBuilder.append("joe");   //data[2][0][0][1] value
      expectedBuilder.append(new byte[]{']', 1, '@'});   //data[2][0][0][2] array indicator, dimensions, component (int)
      expectedBuilder.append(new byte[]{0, 0, 0, 1});   //data[2][0][0][2] length
      expectedBuilder.append(new byte[]{0, 0, 0, 6});   //data[2][0][0][2] value (no header)
      expectedBuilder.append(new byte[]{'\\', 0, 0, 0, 0});   //data[2][0][0][3] id of data

      final byte[] expectedInFile = expectedBuilder.getAllBytes();
      final byte[] actualInFile = mockFile.getAllBytes();
      assertEquals(new String(expectedInFile, StandardCharsets.UTF_8), new String(actualInFile, StandardCharsets.UTF_8));
      assertEquals(Arrays.toString(expectedInFile).replace(", ", ",\n"), Arrays.toString(actualInFile).replace(", ", ",\n"));

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(actualInFile));
      assertEquals(Arrays.deepToString(data), Arrays.deepToString(reader.readObject(Object[][][].class)));
      reader.close();
   }

   @Test
   public void objectArrayOfSupported()
   {
      //This test case exists to validate an edge case since Object.class isn't supported.
      final Object[] data = {1};

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertEquals(Arrays.deepToString(data), Arrays.deepToString(reader.readObject(Object[].class)));
      reader.close();
   }

   @Test
   public void primitiveBooleanArray()
   {
      //This test case exists because boolean[] is an edge case.
      final boolean[] data = new boolean[]{true, false};

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertArrayEquals(data, reader.readObject(boolean[].class));
      reader.close();
   }

   @Test
   public void boxBooleanArray()
   {
      //This test case exists because Boolean[] is an edge case.
      final Boolean[] data = new Boolean[]{true, false};

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertArrayEquals(data, reader.readObject(Boolean[].class));
      reader.close();
   }

   @Test
   public void emptyArray()
   {
      //This test case exists because an empty array can be of a unsupported type.
      final Void[] data = new Void[0];

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      assertArrayEquals(data, reader.readObject(Void[].class));
      reader.close();
   }

   @Test
   public void custom()
   {
      final SimpleHappy data = new SimpleHappy(4);

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));

      final SimpleHappy actual = reader.readObject(SimpleHappy.class);
      assertNotSame(data, actual);
      assertEquals(data, actual);
      reader.close();
   }

   @Test
   public void normalEnum()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(RoundingMode.HALF_DOWN);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));

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
   public void customEnum()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(CustomEnum.One);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));

      assertSame(CustomEnum.One, reader.readObject(CustomEnum.class));
      reader.close();
   }

   @Test
   public void withProxy()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(new ChildImmutable(12));
      writer.writeObject(new ChildMutable("hi"));
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));

      assertEquals(new ChildImmutable(12), reader.readObject(ChildImmutable.class));
      assertEquals(new ChildMutable("hi"), reader.readObject(ChildMutable.class));
      reader.close();
   }

   @Test
   public void rootedGraph()
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
      LOG.debug("writeObject(graph)");
      writer.writeObject(graph);
      LOG.debug("writeObject(root)");
      writer.writeObject(root);
      writer.close();
      LOG.debug("writer.close()\n");

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      LOG.debug("readObject(GraphCallsRegister.class)");
      final GraphCallsRegister actualGraph = reader.readObject(GraphCallsRegister.class);
      LOG.debug("readObject(GraphCallsRegister.Node.class)");
      final GraphCallsRegister.Node actualRoot = reader.readObject(GraphCallsRegister.Node.class);
      reader.close();
      LOG.debug("reader.close()");
      assertNotSame(graph, actualGraph);
      assertEquals(graph, actualGraph);
      assertSame(actualGraph.getRoot(), actualRoot);
   }

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
      assertEquals(root, actualRoot);

      //a -> b -> c then assert that c links to itself. c.links=[b, c]
      final GraphCallsRegister.Node actualClark = actualRoot.links.get(0).links.get(0);
      assertSame(actualClark, actualClark.links.get(1));
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
   public void reflection()
   {
      final ReflectiveClass data = new ReflectiveClass();
      data.field = 0x0afe_babe;

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));

      final ReflectiveClass actual = reader.readObject(ReflectiveClass.class);
      assertNotSame(data, actual);
      assertEquals(data.field, actual.field);
      reader.close();
   }

   @Test
   public void serializable()
   {
      final BigInteger data = BigInteger.TEN;

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));

      final BigInteger actual = reader.readObject();
      assertNotSame(data, actual);  //TEN is not a singleton and BigInteger won't readResolve it to be the same
      assertEquals(data, actual);
      reader.close();
   }
}
