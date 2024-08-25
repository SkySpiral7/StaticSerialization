package com.github.skySpiral7.java.staticSerialization.stream;

import com.github.skySpiral7.java.staticSerialization.exception.ClosedResourceException;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

public class AsynchronousFileReader_UT
{

   @Test
   public void close_doesNothing_whenCalledAgain() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.close_doesNothing_whenCalledAgain.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi");  //make sure it ignores remaining data
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);
      testObject.close();
      testObject.close();
   }

   @Test
   public void read_throws_whenIsClosed() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.read_throws_whenIsClosed.", ".txt");
      tempFile.deleteOnExit();
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);
      testObject.close();
      try
      {
         testObject.readBytes(1);
         fail("Should've thrown");
      }
      catch (final ClosedResourceException actual)
      {
         assertEquals("Can't read from a closed stream", actual.getMessage());
      }
   }

   @Test
   public void read_returnsSome_whenRequestedLessThanAll() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.read_returnsSome_whenRequestedLessThanAll.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi");
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);

      assertEquals("h", new String(testObject.readBytes(1), StandardCharsets.UTF_8));
      assertEquals("i", new String(testObject.readBytes(1), StandardCharsets.UTF_8));
      //the test can only assert that the payload was delivered
      //using breakpoints I think I fixed all deadlocks

      testObject.close();
   }

   @Test
   public void readBytes_returnsAll_whenExact() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.readBytes_returnsAll_whenExact.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi");
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);

      assertEquals("hi", new String(testObject.readBytes(2), StandardCharsets.UTF_8));

      testObject.close();
   }

   @Test
   public void read_returnsShort_whenNotEnough() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.read_returnsShort_whenNotEnough.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi");
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);

      assertEquals("hi", new String(testObject.readBytes(3), StandardCharsets.UTF_8));
      testObject.close();
   }

   @Test
   public void read_returnsEmpty_whenEmpty() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.read_returnsEmpty_whenEmpty.", ".txt");
      tempFile.deleteOnExit();
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);

      assertThat(testObject.readBytes(3), is(new byte[0]));
      testObject.close();
   }

   @Test
   public void readBytesUntil_throws_whenIsClosed() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.readBytesUntil_throws_whenIsClosed.", ".txt");
      tempFile.deleteOnExit();
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);
      testObject.close();
      try
      {
         testObject.readBytesUntil((byte) ',');
         fail("Should've thrown");
      }
      catch (final ClosedResourceException actual)
      {
         assertEquals("Can't read from a closed stream", actual.getMessage());
      }
   }

   @Test
   public void readBytesUntil_returnsSome_whenRequestedLessThanAll() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.readBytesUntil_returnsSome_whenRequestedLessThanAll.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "12,34,");
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);

      assertEquals("12,", new String(testObject.readBytesUntil((byte) ','), StandardCharsets.UTF_8));
      assertEquals("34,", new String(testObject.readBytesUntil((byte) ','), StandardCharsets.UTF_8));

      testObject.close();
   }

   @Test
   public void readBytesUntil_returnsAll_whenExact() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.readBytesUntil_returnsAll_whenExact.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi,");
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);

      assertEquals("hi,", new String(testObject.readBytesUntil((byte) ','), StandardCharsets.UTF_8));

      testObject.close();
   }

   @Test
   public void readBytesUntil_returnsShort_whenNotEnough() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.readBytesUntil_returnsShort_whenNotEnough.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi");
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);

      assertEquals("hi", new String(testObject.readBytesUntil((byte) ','), StandardCharsets.UTF_8));
      testObject.close();
   }

   @Test
   public void readBytesUntil_returnsEmpty_whenEmpty() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.readBytesUntil_returnsEmpty_whenEmpty.", ".txt");
      tempFile.deleteOnExit();
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);

      assertThat(testObject.readBytesUntil((byte) ','), is(new byte[0]));
      testObject.close();
   }
}
