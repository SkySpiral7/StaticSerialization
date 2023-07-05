package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.exception.DeserializationException;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class ReaderValidationStrategy_UT
{
   @Test
   public void readObjectStrictly_throws_whenGotBoolean()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("+-");  //+ is true, - is false

      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObjectStrictly(Object.class);
         fail("Should've thrown");
      }
      catch (IllegalStateException actual)
      {
         assertEquals("Class doesn't match exactly. Expected: java.lang.Object Got: java.lang.Boolean", actual.getMessage());
      }
      try
      {
         testObject.readObjectStrictly(Object.class);
         fail("Should've thrown");
      }
      catch (IllegalStateException actual)
      {
         assertEquals("Class doesn't match exactly. Expected: java.lang.Object Got: java.lang.Boolean", actual.getMessage());
      }
      testObject.close();
   }

   @Test
   public void getClassFromHeader_throws_noCastToBoolean()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("+");  //+ is true

      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObject(String.class);
         fail("Should've thrown");
      }
      catch (final ClassCastException actual)
      {
         assertEquals("java.lang.Boolean cannot be cast to java.lang.String", actual.getMessage());
      }
      testObject.close();
   }

   @Test
   public void getClassFromHeader_header_noSuchClassThrows()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("java.lang.f;");

      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObject(String.class);
         fail("Didn't throw");
      }
      catch (final DeserializationException actual)
      {
         assertEquals(ClassNotFoundException.class, actual.getCause().getClass());
      }

      testObject.close();
   }

   @Test
   public void getClassFromHeader_header_InvalidClassThrows()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("java.;");

      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);

      try
      {
         testObject.readObject(Object.class);
         fail("Didn't throw");
      }
      catch (final DeserializationException actual)
      {
         assertEquals(ClassNotFoundException.class, actual.getCause().getClass());
      }

      testObject.close();
   }

   @Test
   public void getClassFromHeader_returnsArray_whenExpectObject()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{']', 1, '+'});  //array indicator, dimensions, component
      expectedBuilder.append(new byte[]{0, 0, 0, 1});  //length (int)
      expectedBuilder.append("+");  //data
      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      final boolean[] expected = {true};

      final boolean[] actual = testObject.readObject();

      assertEquals(Arrays.toString(expected), Arrays.toString(actual));
      testObject.close();
   }

   @Test
   public void getClassFromHeader_throws_noArrayCast()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("[");
      expectedBuilder.append(new byte[]{1});
      expectedBuilder.append("java.lang.Byte;");
      expectedBuilder.append(new byte[]{0, 0, 0, 0});  //empty array to prove that the check is needed

      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObject(String[].class);
         fail("Didn't throw");
      }
      catch (final ClassCastException actual)
      {
         assertEquals("java.lang.Byte cannot be cast to java.lang.String", actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void getClassFromHeader_throws_primitiveArrayDoesNotCastToBox()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("]");
      expectedBuilder.append(new byte[]{1});
      expectedBuilder.append("~");
      expectedBuilder.append(new byte[]{0, 0, 0, 1, '~', 2});  //length, [0]=2

      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObject(Byte[].class);
         fail("Didn't throw");
      }
      catch (final ClassCastException actual)
      {
         assertEquals("byte cannot be cast to java.lang.Byte", actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void getClassFromHeader_returns_primitiveArray()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("]");
      expectedBuilder.append(new byte[]{1});
      expectedBuilder.append("~");
      expectedBuilder.append(new byte[]{0, 0, 0, 1, 2});  //length, [0]=2

      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertArrayEquals(new byte[]{2}, testObject.readObject(byte[].class));
      testObject.close();
   }

   @Test
   public void getClassFromHeader_throws_whenStreamHasPrimitiveVoid()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("void;");

      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObject(String.class);
         fail("Didn't throw");
      }
      catch (final DeserializationException actual)
      {
         assertEquals(ClassNotFoundException.class, actual.getCause().getClass());
         assertEquals("void", actual.getCause().getMessage());
      }
      testObject.close();
   }

   @Test
   public void getClassFromHeader_header_noCastThrows()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("java.lang.Byte;");

      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObject(String.class);
         fail("Didn't throw");
      }
      catch (final ClassCastException actual)
      {
         assertEquals("java.lang.Byte cannot be cast to java.lang.String", actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void readObjectStrictly_throws_whenExpectedArrayGotNonArray()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      //f doesn't exist but it should throw IllegalStateException. It MUST NOT attempt to load the class
      //meaning that ClassNotFoundException is a failure.
      expectedBuilder.append("f;");

      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObjectStrictly(Object[].class);
         fail("Should've thrown");
      }
      catch (final IllegalStateException actual)
      {
         assertEquals("Class doesn't match exactly. Expected: 1d array of java.lang.Object Got: f", actual.getMessage());
      }
      testObject.close();
   }

   @Test
   public void readObjectStrictly_throws_whenExpectedPrimitiveArrayGotBox()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("[");
      expectedBuilder.append(new byte[]{1});
      expectedBuilder.append("java.lang.Byte;");

      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObjectStrictly(byte[].class);
         fail("Should've thrown");
      }
      catch (final IllegalStateException actual)
      {
         assertEquals("Class doesn't match exactly. Expected: 1d array of primitive java.lang.Byte Got: 1d array of java.lang.Byte",
               actual.getMessage());
      }
      testObject.close();
   }

   @Test
   public void readObjectStrictly_throws_whenExpectedArrayGotDifferentArrayType()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      //f doesn't exist but it should throw IllegalStateException. It MUST NOT attempt to load the class
      //meaning that ClassNotFoundException is a failure.
      expectedBuilder.append("[");
      expectedBuilder.append(new byte[]{1});
      expectedBuilder.append("f;");

      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObjectStrictly(Object[].class);
         fail("Should've thrown");
      }
      catch (final IllegalStateException actual)
      {
         assertEquals("Class doesn't match exactly. Expected: 1d array of java.lang.Object Got: 1d array of f", actual.getMessage());
      }
      testObject.close();
   }

   @Test
   public void readObjectStrictly_throws_whenExpectedArrayGotDifferentDimensions()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("[");
      expectedBuilder.append(new byte[]{2});
      expectedBuilder.append("java.lang.Object;");

      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObjectStrictly(Object[].class);
         fail("Should've thrown");
      }
      catch (final IllegalStateException actual)
      {
         assertEquals("Class doesn't match exactly. Expected: 1d array of java.lang.Object Got: 2d array of java.lang.Object",
               actual.getMessage());
      }
      testObject.close();
   }

   @Test
   public void readObjectStrictly_throws_whenExpectedNonArrayGotArray()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      //f doesn't exist but it should throw IllegalStateException. It MUST NOT attempt to load the class
      //meaning that ClassNotFoundException is a failure.
      expectedBuilder.append("[");
      expectedBuilder.append(new byte[]{1});
      expectedBuilder.append("f;");

      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObjectStrictly(Object.class);
         fail("Should've thrown");
      }
      catch (final IllegalStateException actual)
      {
         assertEquals("Class doesn't match exactly. Expected: java.lang.Object Got: 1d array of f", actual.getMessage());
      }
      testObject.close();
   }

   @Test
   public void readObjectStrictly_throws_whenExpectedNonArrayGotDifferentNonArray()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      //f doesn't exist but it should throw IllegalStateException. It MUST NOT attempt to load the class
      //meaning that ClassNotFoundException is a failure.
      expectedBuilder.append("f;");

      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObjectStrictly(Object.class);
         fail("Should've thrown");
      }
      catch (final IllegalStateException actual)
      {
         assertEquals("Class doesn't match exactly. Expected: java.lang.Object Got: f", actual.getMessage());
      }
      testObject.close();
   }
}
