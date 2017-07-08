package com.github.SkySpiral7.Java.serialization;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.UUID;

import com.github.SkySpiral7.Java.util.FileIoUtil;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;

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

      final byte[] expected = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01,  //UTF-8 length (int)
            (byte) 0x66};
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("*", bytesToString(fileContents, expected.length));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, expected.length)));
   }

   @Test(expected = NullPointerException.class)
   public void writeId_throwsNpe_whenIdNotFound() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectWriterRegistry_UT.TempFile.writeId_throwsNpe_whenIdNotFound.", ".txt");
      tempFile.deleteOnExit();
      testObject.writeId(new Object(), new ObjectStreamWriter(tempFile));
   }

   private String bytesToString(final byte[] data, final int bytesToIgnore)
   {
      return new String(data, 0, (data.length - bytesToIgnore), StandardCharsets.UTF_8);
   }

   private byte[] shortenBytes(final byte[] data, final int bytesToKeep)
   {
      final byte[] smallerData = new byte[bytesToKeep];
      System.arraycopy(data, (data.length - bytesToKeep), smallerData, 0, bytesToKeep);
      return smallerData;
   }

}
