package com.github.skySpiral7.java.staticSerialization;

import java.io.File;
import java.nio.charset.StandardCharsets;

import com.github.skySpiral7.java.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.exception.NoMoreDataException;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

public class ObjectHeaderReader_UT
{
   @Test(expected = NullPointerException.class)
   public void readOverhead_throws_givenNullInput() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectHeaderReader_UT.TempFile.readOverhead_throws_givenNullInput.", ".txt");
      tempFile.deleteOnExit();

      ObjectHeaderReader.readOverhead(null);
   }

   @Test
   public void readOverhead_throws_whenNoData() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectHeaderReader_UT.TempFile.readOverhead_throws_whenNoData.", ".txt");
      tempFile.deleteOnExit();

      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);

      assertEquals(0, reader.remainingBytes());
      try
      {
         ObjectHeaderReader.readOverhead(reader);
         fail("Didn't throw");
      }
      catch (final NoMoreDataException actual)
      {
         assertNull(actual.getMessage());
      }

      reader.close();
   }

   @Test
   public void readOverhead_returnsNullInfo_givenNullInStream() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectHeaderReader_UT.TempFile.readOverhead_returnsNullInfo_givenNullInStream.", ".txt");
      tempFile.deleteOnExit();

      final byte[] fileContents = new byte[]{(byte) ';'};
      FileIoUtil.writeToFile(tempFile, fileContents);
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation(null, null);

      final HeaderInformation actual = ObjectHeaderReader.readOverhead(reader);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readOverhead_returnsTrueInfo_givenTrueInStream() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectHeaderReader_UT.TempFile.readOverhead_returnsTrueInfo_givenTrueInStream.", ".txt");
      tempFile.deleteOnExit();

      final byte[] fileContents = new byte[]{(byte) '+'};
      FileIoUtil.writeToFile(tempFile, fileContents);
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Boolean", Boolean.TRUE);

      final HeaderInformation actual = ObjectHeaderReader.readOverhead(reader);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readOverhead_returnsFalseInfo_givenFalseInStream() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectHeaderReader_UT.TempFile.readOverhead_returnsFalseInfo_givenFalseInStream.", ".txt");
      tempFile.deleteOnExit();

      final byte[] fileContents = new byte[]{(byte) '-'};
      FileIoUtil.writeToFile(tempFile, fileContents);
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Boolean", Boolean.FALSE);

      final HeaderInformation actual = ObjectHeaderReader.readOverhead(reader);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readOverhead_returnsBooleanInfo_givenBooleanObjectInStream() throws Exception
   {
      final File tempFile = File.createTempFile(
            "ObjectHeaderReader_UT.TempFile.readOverhead_returnsBooleanInfo_givenBooleanObjectInStream.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.appendToFile(tempFile, "java.lang.Boolean;".getBytes(StandardCharsets.UTF_8));
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Boolean", null);

      final HeaderInformation actual = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual);

      reader.close();
   }

   @Test
   public void readOverhead_returnsByteInfo_givenByteInStream() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectHeaderReader_UT.TempFile.readOverhead_returnsByteInfo_givenByteInStream.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Byte;~".getBytes(StandardCharsets.UTF_8));
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Byte", null);

      final HeaderInformation actual1 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_returnsShortInfo_givenShortInStream() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectHeaderReader_UT.TempFile.readOverhead_returnsShortInfo_givenShortInStream.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Short;!".getBytes(StandardCharsets.UTF_8));
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Short", null);

      final HeaderInformation actual1 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_returnsIntegerInfo_givenIntegerInStream() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectHeaderReader_UT.TempFile.readOverhead_returnsIntegerInfo_givenIntegerInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Integer;@".getBytes(StandardCharsets.UTF_8));
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Integer", null);

      final HeaderInformation actual1 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_returnsLongInfo_givenLongInStream() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectHeaderReader_UT.TempFile.readOverhead_returnsLongInfo_givenLongInStream.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Long;#".getBytes(StandardCharsets.UTF_8));
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Long", null);

      final HeaderInformation actual1 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_returnsFloatInfo_givenFloatInStream() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectHeaderReader_UT.TempFile.readOverhead_returnsFloatInfo_givenFloatInStream.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Float;%".getBytes(StandardCharsets.UTF_8));
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Float", null);

      final HeaderInformation actual1 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_returnsDoubleInfo_givenDoubleInStream() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectHeaderReader_UT.TempFile.readOverhead_returnsDoubleInfo_givenDoubleInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Double;^".getBytes(StandardCharsets.UTF_8));
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Double", null);

      final HeaderInformation actual1 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_returnsCharacterInfo_givenCharacterInStream() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectHeaderReader_UT.TempFile.readOverhead_returnsCharacterInfo_givenCharacterInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Character;&".getBytes(StandardCharsets.UTF_8));
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Character", null);

      final HeaderInformation actual1 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_returnsStringInfo_givenStringInStream() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectHeaderReader_UT.TempFile.readOverhead_returnsStringInfo_givenStringInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.String;*".getBytes(StandardCharsets.UTF_8));
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.String", null);

      final HeaderInformation actual1 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = ObjectHeaderReader.readOverhead(reader);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readOverhead_throws_givenIncompleteHeader() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectHeaderReader_UT.TempFile.readOverhead_throws_givenIncompleteHeader.", ".txt");
      tempFile.deleteOnExit();
      final byte[] fileContents = {(byte) 'j'};
      FileIoUtil.writeToFile(tempFile, fileContents);
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);

      try
      {
         ObjectHeaderReader.readOverhead(reader);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("Incomplete header", actual.getMessage());
      }

      reader.close();
   }
}
