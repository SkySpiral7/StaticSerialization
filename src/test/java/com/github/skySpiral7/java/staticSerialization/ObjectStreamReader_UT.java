package com.github.skySpiral7.java.staticSerialization;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;

import com.github.skySpiral7.java.exception.NoMoreDataException;
import com.github.skySpiral7.java.staticSerialization.exception.DeserializationException;
import com.github.skySpiral7.java.staticSerialization.exception.InvalidClassException;
import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.testClasses.SimpleHappy;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Ignore;
import org.junit.Test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class ObjectStreamReader_UT
{
   @Test
   public void constructor_throws()
   {
      try
      {
         new ObjectStreamReader(new File(".")).close();
         fail("Didn't throw");
      }
      catch (final IllegalArgumentException actual)
      {
         assertEquals("It is not possible to read file contents of a directory", actual.getMessage());
      }
   }

   @Test
   public void readBytes_throw() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readBytes_throw.", ".txt");
      tempFile.deleteOnExit();
      final byte[] fileContents = {(byte) '!', (byte) 0x0a};
      FileIoUtil.writeToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      try
      {
         testObject.readObject(Short.class);
         fail("Didn't throw");
      }
      catch (final NoMoreDataException actual)
      {
         assertEquals("expected 2 bytes, found 1 bytes", actual.getMessage());
         //this indirectly tests hasData(int) and remainingBytes(). hasData() is tested everywhere
      }

      testObject.close();
   }

   @Test
   public void readObject_allowsCast_givenNoArg() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_allowsCast_givenNoArg.", ".txt");
      tempFile.deleteOnExit();

      final byte[] fileContents = {(byte) '~', (byte) 3};  //~ is byte
      FileIoUtil.writeToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertEquals(Byte.valueOf((byte) 3), testObject.readObject());
      testObject.close();
   }

   @Test
   public void readObjectStrictly_happyPath() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObjectStrictly_happyPath.", ".txt");
      tempFile.deleteOnExit();

      final byte[] fileContents = {(byte) '+', (byte) '-',  //+ is true, - is false
            (byte) '~', (byte) 3};  //~ is byte
      FileIoUtil.writeToFile(tempFile, fileContents);

      final String overhead = "java.math.RoundingMode;";
      FileIoUtil.appendToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8));
      FileIoUtil.appendToFile(tempFile, new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01});

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.readObjectStrictly(Boolean.class));
      assertFalse(testObject.readObjectStrictly(Boolean.class));
      assertEquals(Byte.valueOf((byte) 3), testObject.readObjectStrictly(Byte.class));
      assertEquals(RoundingMode.DOWN, testObject.readObjectStrictly(RoundingMode.class));
      testObject.close();
   }

   @Test
   public void readObjectStrictly_throws_whenGotBoolean() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObjectStrictly_throws_whenGotBoolean.", ".txt");
      tempFile.deleteOnExit();

      final byte[] fileContents = {(byte) '+', (byte) '-'};  //+ is true, - is false
      FileIoUtil.writeToFile(tempFile, fileContents);

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
   @Ignore
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

   @Test
   public void readObject_throw_nullInput() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_throw_nullInput.", ".txt");
      tempFile.deleteOnExit();

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      boolean didCatch = false;
      try
      {
         testObject.readObject(null);
      }
      catch (final NullPointerException actual)
      {
         didCatch = true;
      }
      assertTrue(didCatch);

      testObject.close();
   }

   @Test
   public void readObject_throw_noData() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_throw_noData.", ".txt");
      tempFile.deleteOnExit();

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertFalse(testObject.hasData());
      try
      {
         testObject.readObject(Byte.class);
         fail("Didn't throw");
      }
      catch (final NoMoreDataException actual)
      {
         assertNull(actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void readObject_throw_unknownClass() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_throw_unknownClass.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Object;".getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = {(byte) 2};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      try
      {
         testObject.readObject(Object.class);
         fail("Didn't throw");
      }
      catch (final NotSerializableException actual)
      {
         assertEquals("java.lang.Object", actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void readObject_throw_voidClass() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_throw_voidClass.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "void;".getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = {(byte) 2};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      try
      {
         testObject.readObject(void.class);
         fail("Didn't throw");
      }
      catch (final IllegalArgumentException actual)
      {
         assertEquals("There are no instances of void", actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void readObject_BoxesClassArg_GivenPrimitive() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_BoxesClassArg_GivenPrimitive.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, new byte[]{(byte) '-'});
      FileIoUtil.appendToFile(tempFile, "java.lang.Boolean;".getBytes(StandardCharsets.UTF_8));
      FileIoUtil.appendToFile(tempFile, new byte[]{(byte) 0x01});

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertFalse(testObject.readObject(boolean.class));
      assertTrue(testObject.readObject(boolean.class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_overHead_happy() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_overHead_happy.", ".txt");
      tempFile.deleteOnExit();

      //@formatter:off
		final byte[] fileContents = new byte[] {
				(byte)106, (byte)97, (byte)118, (byte)97, (byte)46,  //"java."
				(byte)108, (byte)97, (byte)110, (byte)103, (byte)46,  //"lang."
				(byte)66, (byte)121, (byte)116, (byte)101,  //"Byte"
				(byte)';',
				(byte)2  //the data
		};
		//@formatter:on
      FileIoUtil.writeToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertEquals(2L, testObject.readObject(Byte.class).longValue());
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_overHead_noSuchClassThrows() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_overHead_noSuchClassThrows.", ".txt");
      tempFile.deleteOnExit();

      //@formatter:off
		final byte[] fileContents = new byte[] {
				(byte)106, (byte)97, (byte)118, (byte)97, (byte)46,  //"java."
				(byte)108, (byte)97, (byte)110,  //"lan"
				(byte)';',
				(byte)2  //the data
		};
		//@formatter:on
      FileIoUtil.writeToFile(tempFile, fileContents);

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
   public void readObject_overHead_upCast() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_overHead_upCast.", ".txt");
      tempFile.deleteOnExit();

      //@formatter:off
		final byte[] fileContents = new byte[] {
				(byte)106, (byte)97, (byte)118, (byte)97, (byte)46,  //"java."
				(byte)108, (byte)97, (byte)110, (byte)103, (byte)46,  //"lang."
				(byte)66, (byte)121, (byte)116, (byte)101,  //"Byte"
				(byte)';',
				(byte)2  //the data
		};
		//@formatter:on
      FileIoUtil.writeToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertEquals(Byte.valueOf((byte) 2), testObject.readObject(Number.class));

      testObject.close();
   }

   @Test
   public void readObject_overHead_null() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_overHead_null.", ".txt");
      tempFile.deleteOnExit();

      final byte[] fileContents = new byte[]{(byte) ';'};
      FileIoUtil.writeToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertNull(testObject.readObject(Byte.class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_overHead_InvalidClassThrows() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_overHead_noClassThrows.", ".txt");
      tempFile.deleteOnExit();

      //@formatter:off
		final byte[] fileContents = new byte[] {
				(byte)106, (byte)97, (byte)118, (byte)97, (byte)46,  //"java."
				(byte)';',
				(byte)2  //the data
		};
		//@formatter:on
      FileIoUtil.writeToFile(tempFile, fileContents);

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
   public void readObject_overHead_noArrayCastThrows() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_overHead_noArrayCastThrows.", ".txt");
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
   public void readObject_overHead_noCastThrows() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_overHead_noCastThrows.", ".txt");
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
   public void readObject_Byte() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_Byte.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Byte;".getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = {(byte) 2, (byte) '~', (byte) 3};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertEquals(2L, testObject.readObject(Byte.class).longValue());
      assertEquals(3L, testObject.readObject(byte.class).longValue());
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_Short() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_Short.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Short;".getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = {(byte) 0x0a, (byte) 0xfe, (byte) '!', (byte) 0x2b, (byte) 0xf1};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertEquals(0x0afeL, testObject.readObject(Short.class).longValue());
      assertEquals(0x2bf1L, testObject.readObject(short.class).longValue());
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_Integer() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_Integer.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Integer;".getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = {(byte) 0x0a, (byte) 0xfe, (byte) 0xba, (byte) 0xbe, (byte) '@', (byte) 0x0a, (byte) 0x1e, (byte) 0xba,
            (byte) 0xb2};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertEquals(0x0afe_babeL, testObject.readObject(Integer.class).longValue());
      assertEquals(0x0a1e_bab2L, testObject.readObject(int.class).longValue());
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_Long() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_Long.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Long;".getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = {(byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x04, (byte) 0x05, (byte) 0x06, (byte) 0x07, (byte) 0x08,
            (byte) '#', (byte) 0x05, (byte) 0x04, (byte) 0x03, (byte) 0x02, (byte) 0x01, (byte) 0x00, (byte) 0x01, (byte) 0x02};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertEquals(0x01020304_05060708L, testObject.readObject(Long.class).longValue());
      assertEquals(0x05040302_01000102L, testObject.readObject(long.class).longValue());
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_Float() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_Float.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Float;".getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = {(byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x04, (byte) '%', (byte) 0xc1, (byte) 0xd2, (byte) 0xe3,
            (byte) 0xf4};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertEquals((Float) Float.intBitsToFloat(0x01020304), testObject.readObject(Float.class));
      assertEquals((Float) Float.intBitsToFloat(0xc1d2e3f4), testObject.readObject(float.class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_Double() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_Double.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Double;".getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = {(byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x04, (byte) 0x05, (byte) 0x06, (byte) 0x07, (byte) 0x08,
            (byte) '^', (byte) 0xa1, (byte) 0xb2, (byte) 0xc3, (byte) 0xd4, (byte) 0xe5, (byte) 0xf6, (byte) 0x17, (byte) 0x08};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertEquals((Double) Double.longBitsToDouble(0x01020304_05060708L), testObject.readObject(Double.class));
      assertEquals((Double) Double.longBitsToDouble(0xa1b2c3d4_e5f61708L), testObject.readObject(double.class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_Boolean() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_Boolean.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.appendToFile(tempFile, "java.lang.Boolean;".getBytes(StandardCharsets.UTF_8));
      FileIoUtil.appendToFile(tempFile, new byte[]{(byte) 0x00});
      FileIoUtil.appendToFile(tempFile, "java.lang.Boolean;".getBytes(StandardCharsets.UTF_8));
      FileIoUtil.appendToFile(tempFile, new byte[]{(byte) 0x01, (byte) '-', (byte) '+'});

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertFalse(testObject.readObject(Boolean.class));
      assertTrue(testObject.readObject(Boolean.class));
      assertFalse(testObject.readObject(boolean.class));
      assertTrue(testObject.readObject(boolean.class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_Character() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_Character.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Character;".getBytes(StandardCharsets.UTF_8));
      FileIoUtil.appendToFile(tempFile, new byte[]{(byte) 0x00, (byte) 0x66});
      //TODO: single character byte[] should be string instead
      FileIoUtil.appendToFile(tempFile, new byte[]{(byte) '&'});
      FileIoUtil.appendToFile(tempFile, new byte[]{(byte) 0x22, (byte) 0x1e});

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertEquals('f', testObject.readObject(Character.class).charValue());
      assertEquals(0x221e, testObject.readObject(char.class).charValue());  //infinity sign is BMP non-private
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_String() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_String.", ".txt");
      tempFile.deleteOnExit();
      //TODO: .getBytes(StandardCharsets.UTF_8) is redundant with current FileIoUtil
      FileIoUtil.writeToFile(tempFile, "java.lang.String;".getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x04,  //UTF-8 length (int)
            (byte) 0x66, (byte) 0xe2, (byte) 0x88, (byte) 0x9e,  //string 1
            (byte) '*',  //shorthand
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01,  //UTF-8 length (int)
            (byte) 0x00};  //string 2
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      //TODO: use UTF-8 in Java files
      assertEquals("f\u221E", testObject.readObject(String.class));  //infinity sign is BMP (3 UTF-8 bytes) non-private
      assertEquals("\u0000", testObject.readObject(String.class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_objectArray() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_objectArray.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "[");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});   //array indicator and dimensions
      FileIoUtil.appendToFile(tempFile, "java.lang.Object;");
      final byte[] fileContents = new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x02,  //length (int)
            (byte) '~', (byte) 0x01,  //each element has overhead
            (byte) '~', (byte) 0x02};
      FileIoUtil.appendToFile(tempFile, fileContents);
      final Object[] expected = {(byte) 1, (byte) 2};

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertArrayEquals(expected, testObject.readObject(Object[].class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_boxArray() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_boxArray.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "[");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});   //array indicator and dimensions
      FileIoUtil.appendToFile(tempFile, "java.lang.Byte;");
      final byte[] fileContents = new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x02,  //length (int)
            (byte) '~', (byte) 0x01,  //each element has overhead
            (byte) '~', (byte) 0x02};
      FileIoUtil.appendToFile(tempFile, fileContents);
      final Byte[] expected = {1, 2};

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertArrayEquals(expected, testObject.readObject(Byte[].class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   @Ignore
   public void readObject_primitiveArray() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_primitiveArray.", ".txt");
      tempFile.deleteOnExit();
      final byte[] fileContents = new byte[]{(byte) '[', (byte) 1,   //array indicator and dimensions
            (byte) '~',  //byte
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x02,  //length (int)
            (byte) '~', (byte) 0x01,  //each element has overhead
            (byte) '~', (byte) 0x02};
      FileIoUtil.writeToFile(tempFile, fileContents);
      final byte[] expected = {1, 2};

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertArrayEquals(expected, testObject.readObject(byte[].class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   @Ignore
   public void readObject_2dArray() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_2dArray.", ".txt");
      tempFile.deleteOnExit();
      final byte[] fileContents = new byte[]{(byte) '[', (byte) 2,   //array indicator and dimensions
            (byte) '~',  //byte
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x02,  //length (int)
            (byte) '[', (byte) 1,   //first element
            (byte) '~',  //byte
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01,  //length (int)
            (byte) '~', (byte) 0x01, (byte) ';'};   //second element
      FileIoUtil.writeToFile(tempFile, fileContents);
      final byte[][] expected = {{1}, null};

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertArrayEquals(expected, testObject.readObject(byte[][].class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   @Ignore
   public void readObject_primitiveBooleanArray() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_primitiveBooleanArray.", ".txt");
      tempFile.deleteOnExit();
      final byte[] fileContents = new byte[]{(byte) '[', (byte) 1,   //array indicator and dimensions
            (byte) '+',  //boolean
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01,  //length (int)
            (byte) '+'};
      FileIoUtil.writeToFile(tempFile, fileContents);
      final boolean[] expected = {true};

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertArrayEquals(expected, testObject.readObject(boolean[].class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_boxBooleanArray() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_boxBooleanArray.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "[");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});   //array indicator and dimensions
      FileIoUtil.appendToFile(tempFile, "java.lang.Boolean;");
      final byte[] fileContents = new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01,  //length (int)
            (byte) '-'};
      FileIoUtil.appendToFile(tempFile, fileContents);
      final Boolean[] expected = {false};

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertArrayEquals(expected, testObject.readObject(Boolean[].class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_emptyArray() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_emptyArray.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "[");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});   //array indicator and dimensions
      FileIoUtil.appendToFile(tempFile, "java.lang.Void;");
      final byte[] fileContents = new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00};  //length (int)
      FileIoUtil.appendToFile(tempFile, fileContents);
      final Void[] expected = new Void[0];

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertArrayEquals(expected, testObject.readObject(Void[].class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_enum() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_enum.", ".txt");
      tempFile.deleteOnExit();
      final String overhead = "java.math.RoundingMode;";
      FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertSame(RoundingMode.DOWN, testObject.readObject(RoundingMode.class));

      testObject.close();
   }

   @Test
   public void readObject_enum_OrdinalNotFound() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_enum_OrdinalNotFound.", ".txt");
      tempFile.deleteOnExit();
      final String overhead = "java.math.RoundingMode;";
      FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x0a};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      try
      {
         testObject.readObject(RoundingMode.class);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("java.math.RoundingMode.values()[10] doesn't exist. Actual length: 8", actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void readObject_custom_happy() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_custom_happy.", ".txt");
      tempFile.deleteOnExit();
      final String overhead = "com.github.skySpiral7.java.staticSerialization.testClasses.SimpleHappy;";
      FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = new byte[]{(byte) '@', (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x04};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      final SimpleHappy readData = testObject.readObject(SimpleHappy.class);
      assertEquals(4, readData.smileyStickersCount);
      assertFalse(testObject.hasData());

      testObject.close();
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
         fail("Should not be called");
      }
   }

   @Test
   public void readObject_customEnum() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_customEnum.", ".txt");
      tempFile.deleteOnExit();
      final String overhead = "com.github.skySpiral7.java.staticSerialization.ObjectStreamReader_UT$CustomEnum;*";
      FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x03};
      FileIoUtil.appendToFile(tempFile, fileContents);
      FileIoUtil.appendToFile(tempFile, "One".getBytes(StandardCharsets.UTF_8));

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertSame(CustomEnum.One, testObject.readObject(CustomEnum.class));

      testObject.close();
   }

   @Test
   public void readObject_custom_throw_noReader() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_custom_throw_noReader.", ".txt");
      tempFile.deleteOnExit();
      final String overhead = "com.github.skySpiral7.java.staticSerialization.ObjectStreamReader_UT$1NoReader;";
      FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x04};
      FileIoUtil.appendToFile(tempFile, fileContents);

      abstract class NoReader implements StaticSerializable
      {}  //abstract and no writer doesn't matter

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      try
      {
         testObject.readObject(NoReader.class);
         fail("Didn't throw");
      }
      catch (final InvalidClassException actual)
      {
         assertEquals("com.github.skySpiral7.java.staticSerialization.ObjectStreamReader_UT$1NoReader"
                      + " implements StaticSerializable but doesn't define readFromStream", actual.getMessage());
      }

      testObject.close();
   }

   private abstract static class NonPublicReader implements StaticSerializable
   {
      @SuppressWarnings("unused")
      protected static NonPublicReader readFromStream(final ObjectStreamReader in)
      {
         return null;
      }
   }

   @Test
   public void readObject_custom_throw_nonPublic() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_custom_throw_nonPublic.", ".txt");
      tempFile.deleteOnExit();
      final String overhead = "com.github.skySpiral7.java.staticSerialization.ObjectStreamReader_UT$NonPublicReader;";
      FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x04};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      try
      {
         testObject.readObject(NonPublicReader.class);
         fail("Didn't throw");
      }
      catch (final InvalidClassException actual)
      {
         assertEquals("com.github.skySpiral7.java.staticSerialization.ObjectStreamReader_UT$NonPublicReader.readFromStream"
                      + " must be public static", actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void readObject_custom_throw_nonStatic() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_custom_throw_nonStatic.", ".txt");
      tempFile.deleteOnExit();
      final String overhead = "com.github.skySpiral7.java.staticSerialization.ObjectStreamReader_UT$1LocalNonStaticReader;";
      FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x04};
      FileIoUtil.appendToFile(tempFile, fileContents);

      abstract class LocalNonStaticReader implements StaticSerializable
      {
         @SuppressWarnings("unused")
         public LocalNonStaticReader readFromStream(final ObjectStreamReader in)
         {
            return null;
         }
      }

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      try
      {
         testObject.readObject(LocalNonStaticReader.class);
         fail("Didn't throw");
      }
      catch (final InvalidClassException actual)
      {
         assertEquals("com.github.skySpiral7.java.staticSerialization.ObjectStreamReader_UT$1LocalNonStaticReader.readFromStream"
                      + " must be public static", actual.getMessage());
      }

      testObject.close();
   }

   private abstract static class ThrowingReader implements StaticSerializable
   {
      @SuppressWarnings("unused")
      public static ThrowingReader readFromStream(final ObjectStreamReader in)
      {
         throw new UnsupportedOperationException();
      }
   }

   @Test
   public void readObject_custom_throw_throwingReader() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_custom_throw_throwingReader.", ".txt");
      tempFile.deleteOnExit();
      final String overhead = "com.github.skySpiral7.java.staticSerialization.ObjectStreamReader_UT$ThrowingReader;";
      FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x04};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      try
      {
         testObject.readObject(ThrowingReader.class);
         fail("Didn't throw");
      }
      catch (final DeserializationException actual)
      {
         assertEquals(InvocationTargetException.class, actual.getCause().getClass());
         assertEquals(UnsupportedOperationException.class, actual.getCause().getCause().getClass());
      }

      testObject.close();
   }

   @Test
   public void readObject_Serializable() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_Serializable.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.math.BigInteger;".getBytes(StandardCharsets.UTF_8));
      final BigInteger data = BigInteger.TEN;
      final byte[] javaData = ObjectStreamWriter.javaSerialize(data);
      assert (javaData.length < 256);  //currently 203. Possible for the length to change after a Java release
      FileIoUtil.appendToFile(tempFile, new byte[]{0, 0, 0, (byte) javaData.length});
      FileIoUtil.appendToFile(tempFile, javaData);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertEquals(data, testObject.readObject());

      testObject.close();
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
      public void writeToStream(final ObjectStreamWriter writer){}
   }

   @Test
   public void readFieldsReflectively() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readFieldsReflectively.", ".txt");
      tempFile.deleteOnExit();
      final String overhead = "com.github.skySpiral7.java.staticSerialization.ObjectStreamReader_UT$ReflectiveClass;java.lang.Integer;";
      FileIoUtil.writeToFile(tempFile, overhead.getBytes(StandardCharsets.UTF_8));
      final byte[] fileContents = {(byte) 0x0a, (byte) 0xfe, (byte) 0xba, (byte) 0xbe};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertEquals(0x0afe_babeL, testObject.readObject(ReflectiveClass.class).field);

      testObject.close();
   }

}