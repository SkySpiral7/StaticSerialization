package com.github.skySpiral7.java.staticSerialization.strategy;

import java.io.File;

import com.github.skySpiral7.java.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class HeaderSerializableStrategy_UT
{
   @Test(expected = NullPointerException.class)
   public void readOverhead_throws_givenNullInput() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_throws_givenNullInput.", ".txt");
      tempFile.deleteOnExit();

      HeaderSerializableStrategy.readOverhead(null);
   }

   @Test
   public void readOverhead_throws_whenNoArrayDimensions() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_throws_whenNoArrayDimensions.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "[");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);

      try
      {
         HeaderSerializableStrategy.readOverhead(reader);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("Incomplete header: no array dimensions", actual.getMessage());
      }

      reader.close();
   }

   @Test
   public void readOverhead_throws_whenNoArrayComponent() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_throws_whenNoArrayComponent.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "[a");  //'a' is a lot of dimensions
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);

      try
      {
         HeaderSerializableStrategy.readOverhead(reader);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("Incomplete header: no array component type", actual.getMessage());
      }

      reader.close();
   }

   @Test
   public void readOverhead_throws_whenArrayComponentIsNull() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_throws_whenArrayComponentIsNull.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "[a;");  //'a' is a lot of dimensions
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);

      try
      {
         HeaderSerializableStrategy.readOverhead(reader);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("header's array component type can't be null or false", actual.getMessage());
      }

      reader.close();
   }

   @Test
   public void readOverhead_throws_whenArrayComponentIsFalse() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_throws_whenArrayComponentIsFalse.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "[a-");  //'a' is a lot of dimensions
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);

      try
      {
         HeaderSerializableStrategy.readOverhead(reader);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("header's array component type can't be null or false", actual.getMessage());
      }

      reader.close();
   }

   @Test
   public void readOverhead_returns_givenBooleanArrayInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_returns_givenBooleanArrayInStream.",
            ".txt");
      tempFile.deleteOnExit();

      FileIoUtil.writeToFile(tempFile, "[");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});
      FileIoUtil.appendToFile(tempFile, "+");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation(Boolean.class.getName(), null, 1);

      final HeaderInformation actual = HeaderSerializableStrategy.readOverhead(reader);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readOverhead_returnsNullInfo_givenNullInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_returnsNullInfo_givenNullInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, ";");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation(null, null, 0);

      final HeaderInformation actual = HeaderSerializableStrategy.readOverhead(reader);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readOverhead_returnsTrueInfo_givenTrueInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_returnsTrueInfo_givenTrueInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "+");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Boolean", Boolean.TRUE, 0);

      final HeaderInformation actual = HeaderSerializableStrategy.readOverhead(reader);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readOverhead_returnsFalseInfo_givenFalseInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_returnsFalseInfo_givenFalseInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "-");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Boolean", Boolean.FALSE, 0);

      final HeaderInformation actual = HeaderSerializableStrategy.readOverhead(reader);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readOverhead_returnsBooleanInfo_givenBooleanObjectInStream() throws Exception
   {
      final File tempFile = File.createTempFile(
            "HeaderSerializableStrategy_UT.TempFile.readOverhead_returnsBooleanInfo_givenBooleanObjectInStream.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Boolean;");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Boolean", null, 0);

      final HeaderInformation actual = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual);

      reader.close();
   }

   @Test
   public void readOverhead_returnsByteInfo_givenByteInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_returnsByteInfo_givenByteInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Byte;~");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Byte", null, 0);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_returnsShortInfo_givenShortInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_returnsShortInfo_givenShortInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Short;!");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Short", null, 0);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_returnsIntegerInfo_givenIntegerInStream() throws Exception
   {
      final File tempFile = File.createTempFile(
            "HeaderSerializableStrategy_UT.TempFile.readOverhead_returnsIntegerInfo_givenIntegerInStream.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Integer;@");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Integer", null, 0);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_returnsLongInfo_givenLongInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_returnsLongInfo_givenLongInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Long;#");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Long", null, 0);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_returnsFloatInfo_givenFloatInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_returnsFloatInfo_givenFloatInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Float;%");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Float", null, 0);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_returnsDoubleInfo_givenDoubleInStream() throws Exception
   {
      final File tempFile = File.createTempFile(
            "HeaderSerializableStrategy_UT.TempFile.readOverhead_returnsDoubleInfo_givenDoubleInStream.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Double;^");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Double", null, 0);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_returnsCharacterInfo_givenCharacterInStream() throws Exception
   {
      final File tempFile = File.createTempFile(
            "HeaderSerializableStrategy_UT.TempFile.readOverhead_returnsCharacterInfo_givenCharacterInStream.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Character;&");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Character", null, 0);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_returnsStringInfo_givenStringInStream() throws Exception
   {
      final File tempFile = File.createTempFile(
            "HeaderSerializableStrategy_UT.TempFile.readOverhead_returnsStringInfo_givenStringInStream.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.String;*");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.String", null, 0);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_returns_givenByteArrayInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_returns_givenByteArrayInStream.",
            ".txt");
      tempFile.deleteOnExit();

      FileIoUtil.writeToFile(tempFile, "[");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});
      FileIoUtil.appendToFile(tempFile, "~");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation(Byte.class.getName(), null, 1);

      final HeaderInformation actual = HeaderSerializableStrategy.readOverhead(reader);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readOverhead_throws_whenHeaderNoTerminated() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_throws_whenHeaderNoTerminated.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "j");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);

      try
      {
         HeaderSerializableStrategy.readOverhead(reader);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("Incomplete header: class name not terminated", actual.getMessage());
      }

      reader.close();
   }

   @Test
   public void readOverhead_returnsObjectInfo_givenObjectInStream() throws Exception
   {
      final File tempFile = File.createTempFile(
            "HeaderSerializableStrategy_UT.TempFile.readOverhead_returnsObjectInfo_givenObjectInStream.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Object;");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Object", null, 0);

      final HeaderInformation actual = HeaderSerializableStrategy.readOverhead(reader);
      assertEquals(expected, actual);

      reader.close();
   }

   @Test
   public void readOverhead_returns_givenObjectArrayInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readOverhead_returns_givenObjectArrayInStream.",
            ".txt");
      tempFile.deleteOnExit();

      FileIoUtil.writeToFile(tempFile, "[");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});
      FileIoUtil.appendToFile(tempFile, "java.lang.Object;");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation(Object.class.getName(), null, 1);

      final HeaderInformation actual = HeaderSerializableStrategy.readOverhead(reader);

      assertEquals(expected, actual);
      reader.close();
   }
}
