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
   public void readsFromFile() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.readsFromFile.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi");
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);

      assertEquals("hi", new String(testObject.readBytes(2), StandardCharsets.UTF_8));
      //the test can only assert that the payload was delivered
      //using breakpoints I think I fixed all deadlocks

      testObject.close();
   }

   @Test
   public void closeTwiceDoesNothing() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.closeTwiceDoesNothing.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi");  //make sure it ignores remaining data
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);
      testObject.close();
      testObject.close();
   }

   @Test
   public void readAfterCloseThrows() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.readAfterCloseThrows.", ".txt");
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
   public void readEndOfFileTooShort() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.readEndOfFileTooShort.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi");
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);

      assertEquals("hi", new String(testObject.readBytes(3), StandardCharsets.UTF_8));
      testObject.close();
   }

   @Test
   public void readEndOfFileEmpty() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.readEndOfFileEmpty.", ".txt");
      tempFile.deleteOnExit();
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);

      assertThat(testObject.readBytes(3), is(new byte[0]));
      testObject.close();
   }
}
