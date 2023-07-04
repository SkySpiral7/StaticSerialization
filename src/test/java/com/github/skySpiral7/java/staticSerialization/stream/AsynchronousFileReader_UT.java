package com.github.skySpiral7.java.staticSerialization.stream;

import java.io.File;
import java.io.IOException;

import com.github.skySpiral7.java.staticSerialization.exception.ClosedResourceException;
import com.github.skySpiral7.java.staticSerialization.exception.NoMoreDataException;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class AsynchronousFileReader_UT
{

   @Test
   public void readsFromFile() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.readsFromFile.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi");
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);

      assertEquals("hi", testObject.readString(2));
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
      }
      catch (final ClosedResourceException actual)
      {
         assertEquals("Can't read from a closed stream", actual.getMessage());
      }
   }

   @Test
   public void readEndOfFileThrows() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileReader_UT.TempFile.readEndOfFileThrows.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "hi");
      final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);
      try
      {
         testObject.readBytes(3);
      }
      catch (final NoMoreDataException actual)
      {
         assertEquals("expected 3 bytes, found 2 bytes", actual.getMessage());
      }
      testObject.close();
   }
}
