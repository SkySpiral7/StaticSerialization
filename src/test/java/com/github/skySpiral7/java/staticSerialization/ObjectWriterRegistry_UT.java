package com.github.skySpiral7.java.staticSerialization;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.UUID;

import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

public class ObjectWriterRegistry_UT
{
   private ObjectWriterRegistry testObject;

   @Before
   public void setUp()
   {
      testObject = new ObjectWriterRegistry();
   }

   @Test(expected = NullPointerException.class)
   public void registerObject_throwsNpe_givenNullId()
   {
      testObject.registerObject(null, "");
   }

   @Test(expected = NullPointerException.class)
   public void registerObject_throwsNpe_givenNullValue()
   {
      testObject.registerObject("", null);
   }

   @Test(expected = NullPointerException.class)
   public void registerObject_object_throwsNpe_givenNullValue()
   {
      testObject.registerObject(null);
   }

   @Test(expected = NullPointerException.class)
   public void getId_throwsNpe_givenNullValue()
   {
      testObject.getId(null);
   }

   @Test
   public void getId_returnsId_withAutoId()
   {
      final String data = "test me";
      final String id = testObject.registerObject(data);
      assertSame(id, testObject.getId(data));
   }

   @Test
   public void getId_returnsId_withManualId()
   {
      final String data = "test me";
      final String id = UUID.randomUUID().toString();

      testObject.registerObject(id, data);
      assertSame(id, testObject.getId(data));
   }

   @Test(expected = NullPointerException.class)
   public void writeId_throwsNpe_givenNullInstance() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectWriterRegistry_UT.TempFile.writeId_throwsNpe_givenNullInstance.", ".txt");
      tempFile.deleteOnExit();
      testObject.writeId(null, new ObjectStreamWriter(tempFile));
   }

   @Test(expected = NullPointerException.class)
   public void writeId_throwsNpe_givenNullWriter()
   {
      testObject.writeId("", null);
   }

   @Test
   public void writeId_writesId_whenIdExists() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectWriterRegistry_UT.TempFile.writeId_writesId_whenIdExists.", ".txt");
      tempFile.deleteOnExit();
      final String id = "f";
      final Object instance = new Object();

      testObject.registerObject(id, instance);

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      testObject.writeId(instance, writer);
      writer.close();

      final byte[] expected = {(byte) '*',  //type of id is string
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01,  //UTF-8 length (int)
            (byte) 'f'};  //id
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertArrayEquals(expected, fileContents);
   }

   @Test(expected = NullPointerException.class)
   public void writeId_throwsNpe_whenIdNotFound() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectWriterRegistry_UT.TempFile.writeId_throwsNpe_whenIdNotFound.", ".txt");
      tempFile.deleteOnExit();
      testObject.writeId(new Object(), new ObjectStreamWriter(tempFile));
   }

   @Test(expected = NullPointerException.class)
   public void shouldNotWrite_throwsNpe_givenNullInstance() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectWriterRegistry_UT.TempFile.shouldNotWrite_throwsNpe_givenNullInstance.", ".txt");
      tempFile.deleteOnExit();
      testObject.shouldNotWrite(null, new ObjectStreamWriter(tempFile));
   }

   @Test(expected = NullPointerException.class)
   public void shouldNotWrite_throwsNpe_givenNullWriter()
   {
      testObject.shouldNotWrite("", null);
   }

   @Test
   public void shouldNotWrite_writesIdAndReturnsTrue_whenIdExists() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectWriterRegistry_UT.TempFile.shouldNotWrite_writesIdAndReturnsTrue_whenIdExists.",
            ".txt");
      tempFile.deleteOnExit();
      final String id = "f";
      final Object instance = new Object();

      testObject.registerObject(id, instance);

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      assertTrue(testObject.shouldNotWrite(instance, writer));
      writer.close();

      final byte[] expected = {(byte) '*',  //type of id is string
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01,  //UTF-8 length (int)
            (byte) 'f'};  //id
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertArrayEquals(expected, fileContents);
   }

   @Test
   public void shouldNotWrite_registersWritesIdAndReturnsFalse_whenNoId() throws IOException
   {
      final File tempFile = File.createTempFile(
            "ObjectWriterRegistry_UT.TempFile.shouldNotWrite_registersWritesIdAndReturnsFalse_whenNoId.", ".txt");
      tempFile.deleteOnExit();
      final Object instance = new Object();

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      assertFalse(testObject.shouldNotWrite(instance, writer));
      writer.close();

      final String id = testObject.getId(instance);
      assertNotNull(id);

      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      final byte[] header = {(byte) '*',  //type of id is string
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 36};  //UTF-8 length (int)
      baos.write(header);
      baos.write(id.getBytes(StandardCharsets.UTF_8));
      final byte[] expected = baos.toByteArray();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertArrayEquals(expected, fileContents);
   }
}
