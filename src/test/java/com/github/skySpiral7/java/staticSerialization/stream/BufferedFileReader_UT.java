package com.github.skySpiral7.java.staticSerialization.stream;

import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class BufferedFileReader_UT
{

   @Test
   public void close_doesNothing_whenCalledAgain() throws IOException
   {
      final File tempFile = File.createTempFile("BufferedFileReader_UT.TempFile.close_doesNothing_whenCalledAgain.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi");  //make sure it ignores remaining data
      final BufferedFileReader testObject = new BufferedFileReader(tempFile);
      testObject.close();
      testObject.close();
   }

   @Test
   public void read_throws_whenIsClosed() throws IOException
   {
      final File tempFile = File.createTempFile("BufferedFileReader_UT.TempFile.read_throws_whenIsClosed.", ".txt");
      tempFile.deleteOnExit();
      final BufferedFileReader testObject = new BufferedFileReader(tempFile);
      testObject.close();

      assertThrows(UncheckedIOException.class, () -> testObject.readBytes(1));
   }

   @Test
   public void read_returnsSome_whenRequestedLessThanAll() throws IOException
   {
      final File tempFile = File.createTempFile("BufferedFileReader_UT.TempFile.read_returnsSome_whenRequestedLessThanAll.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi");
      final BufferedFileReader testObject = new BufferedFileReader(tempFile);

      assertEquals("h", new String(testObject.readBytes(1), StandardCharsets.UTF_8));
      assertEquals("i", new String(testObject.readBytes(1), StandardCharsets.UTF_8));

      testObject.close();
   }

   @Test
   public void readBytes_returnsAll_whenExact() throws IOException
   {
      final File tempFile = File.createTempFile("BufferedFileReader_UT.TempFile.readBytes_returnsAll_whenExact.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi");
      final BufferedFileReader testObject = new BufferedFileReader(tempFile);

      assertEquals("hi", new String(testObject.readBytes(2), StandardCharsets.UTF_8));

      testObject.close();
   }

   @Test
   public void read_returnsShort_whenNotEnough() throws IOException
   {
      final File tempFile = File.createTempFile("BufferedFileReader_UT.TempFile.read_returnsShort_whenNotEnough.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi");
      final BufferedFileReader testObject = new BufferedFileReader(tempFile);

      assertEquals("hi", new String(testObject.readBytes(3), StandardCharsets.UTF_8));
      testObject.close();
   }

   @Test
   public void read_returnsEmpty_whenEmpty() throws IOException
   {
      final File tempFile = File.createTempFile("BufferedFileReader_UT.TempFile.read_returnsEmpty_whenEmpty.", ".txt");
      tempFile.deleteOnExit();
      final BufferedFileReader testObject = new BufferedFileReader(tempFile);

      assertThat(testObject.readBytes(3), is(new byte[0]));
      testObject.close();
   }

   @Test
   public void readBytesUntil_throws_whenIsClosed() throws IOException
   {
      final File tempFile = File.createTempFile("BufferedFileReader_UT.TempFile.readBytesUntil_throws_whenIsClosed.", ".txt");
      tempFile.deleteOnExit();
      final BufferedFileReader testObject = new BufferedFileReader(tempFile);
      testObject.close();

      assertThrows(UncheckedIOException.class, () -> testObject.readBytesUntil((byte) ','));
   }

   @Test
   public void readBytesUntil_returnsSome_whenRequestedLessThanAll() throws IOException
   {
      final File tempFile = File.createTempFile("BufferedFileReader_UT.TempFile.readBytesUntil_returnsSome_whenRequestedLessThanAll.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "12,34,");
      final BufferedFileReader testObject = new BufferedFileReader(tempFile);

      assertEquals("12,", new String(testObject.readBytesUntil((byte) ','), StandardCharsets.UTF_8));
      assertEquals("34,", new String(testObject.readBytesUntil((byte) ','), StandardCharsets.UTF_8));

      testObject.close();
   }

   @Test
   public void readBytesUntil_returnsAll_whenExact() throws IOException
   {
      final File tempFile = File.createTempFile("BufferedFileReader_UT.TempFile.readBytesUntil_returnsAll_whenExact.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi,");
      final BufferedFileReader testObject = new BufferedFileReader(tempFile);

      assertEquals("hi,", new String(testObject.readBytesUntil((byte) ','), StandardCharsets.UTF_8));

      testObject.close();
   }

   @Test
   public void readBytesUntil_returnsShort_whenNotEnough() throws IOException
   {
      final File tempFile = File.createTempFile("BufferedFileReader_UT.TempFile.readBytesUntil_returnsShort_whenNotEnough.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi");
      final BufferedFileReader testObject = new BufferedFileReader(tempFile);

      assertEquals("hi", new String(testObject.readBytesUntil((byte) ','), StandardCharsets.UTF_8));
      testObject.close();
   }

   @Test
   public void readBytesUntil_returnsEmpty_whenEmpty() throws IOException
   {
      final File tempFile = File.createTempFile("BufferedFileReader_UT.TempFile.readBytesUntil_returnsEmpty_whenEmpty.", ".txt");
      tempFile.deleteOnExit();
      final BufferedFileReader testObject = new BufferedFileReader(tempFile);

      assertThat(testObject.readBytesUntil((byte) ','), is(new byte[0]));
      testObject.close();
   }
}
