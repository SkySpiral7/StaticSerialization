package com.github.skySpiral7.java.staticSerialization.strategy;

import java.io.File;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.exception.DeserializationException;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class ReaderValidationStrategy_UT
{
   @Test
   public void readObjectStrictly_throws_whenGotBoolean() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObjectStrictly_throws_whenGotBoolean.", ".txt");
      tempFile.deleteOnExit();

      FileIoUtil.writeToFile(tempFile, "+-");  //+ is true, - is false

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
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
   public void readObject_throws_noCastToBoolean() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_throws_noCastToBoolean.", ".txt");
      tempFile.deleteOnExit();

      FileIoUtil.writeToFile(tempFile, "+");  //+ is true

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      try
      {
         final Object myThing = testObject.readObject(String.class);
         System.out.println(myThing);
         fail("Should've thrown");
      }
      catch (final ClassCastException actual)
      {
         assertEquals("java.lang.Boolean cannot be cast to java.lang.String", actual.getMessage());
      }
      testObject.close();
   }

   @Test
   public void readObject_header_noSuchClassThrows() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_header_noSuchClassThrows.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.f;");

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
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
   public void readObject_header_InvalidClassThrows() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_header_noClassThrows.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.;");

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);

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
   public void readObject_throws_noArrayCast() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_header_noArrayCastThrows.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "[");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});
      FileIoUtil.appendToFile(tempFile, "java.lang.Byte;");
      FileIoUtil.appendToFile(tempFile, new byte[]{0, 0, 0, 0});  //empty array to prove that the check is needed

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
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
   public void readObject_throws_primitiveArrayDoesNotCastToBox() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_throws_primitiveArrayDoesNotCastToBox.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "]");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});
      FileIoUtil.appendToFile(tempFile, "~");
      FileIoUtil.appendToFile(tempFile, new byte[]{0, 0, 0, 1, '~', 2});  //length, [0]=2

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
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
   public void readObject_returns_primitiveArray() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_returns_primitiveArray.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "]");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});
      FileIoUtil.appendToFile(tempFile, "~");
      FileIoUtil.appendToFile(tempFile, new byte[]{0, 0, 0, 1, '~', 2});  //length, [0]=2

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertArrayEquals(new byte[]{2}, testObject.readObject(byte[].class));
      testObject.close();
   }

   @Test
   public void readObject_throws_whenStreamHasPrimitiveVoid() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_throws_whenStreamHasPrimitiveVoid.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "void;");

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
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
   public void readObject_header_noCastThrows() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_header_noCastThrows.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Byte;");

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
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
   public void readObjectStrictly_throws_whenExpectedArrayGotNonArray() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObjectStrictly_throws_whenExpectedArrayGotNonArray.",
            ".txt");
      tempFile.deleteOnExit();

      //f doesn't exist but it should throw IllegalStateException. It MUST NOT attempt to load the class
      //meaning that ClassNotFoundException is a failure.
      FileIoUtil.writeToFile(tempFile, "f;");

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
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
   public void readObjectStrictly_throws_whenExpectedPrimitiveArrayGotBox() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObjectStrictly_throws_whenExpectedArrayGotNonArray.",
            ".txt");
      tempFile.deleteOnExit();

      FileIoUtil.writeToFile(tempFile, "[");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});
      FileIoUtil.appendToFile(tempFile, "java.lang.Byte;");

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
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
   public void readObjectStrictly_throws_whenExpectedArrayGotDifferentArrayType() throws Exception
   {
      final File tempFile = File.createTempFile(
            "ObjectStreamReader_UT.TempFile.readObjectStrictly_throws_whenExpectedArrayGotDifferentArrayType.", ".txt");
      tempFile.deleteOnExit();

      //f doesn't exist but it should throw IllegalStateException. It MUST NOT attempt to load the class
      //meaning that ClassNotFoundException is a failure.
      FileIoUtil.writeToFile(tempFile, "[");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});
      FileIoUtil.appendToFile(tempFile, "f;");

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
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
   public void readObjectStrictly_throws_whenExpectedArrayGotDifferentDimensions() throws Exception
   {
      final File tempFile = File.createTempFile(
            "ObjectStreamReader_UT.TempFile.readObjectStrictly_throws_whenExpectedArrayGotDifferentDimensions.", ".txt");
      tempFile.deleteOnExit();

      FileIoUtil.writeToFile(tempFile, "[");
      FileIoUtil.appendToFile(tempFile, new byte[]{2});
      FileIoUtil.appendToFile(tempFile, "java.lang.Object;");

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
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
   public void readObjectStrictly_throws_whenExpectedNonArrayGotArray() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObjectStrictly_throws_whenExpectedNonArrayGotArray.",
            ".txt");
      tempFile.deleteOnExit();

      //f doesn't exist but it should throw IllegalStateException. It MUST NOT attempt to load the class
      //meaning that ClassNotFoundException is a failure.
      FileIoUtil.writeToFile(tempFile, "[");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});
      FileIoUtil.appendToFile(tempFile, "f;");

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
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
   public void readObjectStrictly_throws_whenExpectedNonArrayGotDifferentNonArray() throws Exception
   {
      final File tempFile = File.createTempFile(
            "ObjectStreamReader_UT.TempFile.readObjectStrictly_throws_whenExpectedNonArrayGotDifferentNonArray.", ".txt");
      tempFile.deleteOnExit();

      //f doesn't exist but it should throw IllegalStateException. It MUST NOT attempt to load the class
      //meaning that ClassNotFoundException is a failure.
      FileIoUtil.writeToFile(tempFile, "f;");

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
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
