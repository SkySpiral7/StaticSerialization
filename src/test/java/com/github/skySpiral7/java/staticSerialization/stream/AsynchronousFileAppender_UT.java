package com.github.skySpiral7.java.staticSerialization.stream;

import com.github.skySpiral7.java.staticSerialization.exception.ClosedResourceException;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

public class AsynchronousFileAppender_UT
{
   @Test
   public void constructor_clears() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileAppender_UT.TempFile.constructor_clears.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "test");
      new AsynchronousFileAppender(tempFile).close();
      assertEquals("", FileIoUtil.readTextFile(tempFile));
   }

   @Test
   public void writesToFile() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileAppender_UT.TempFile.writesToFile.", ".txt");
      tempFile.deleteOnExit();
      final AsynchronousFileAppender testObject = new AsynchronousFileAppender(tempFile);

      testObject.append("hi");
      testObject.flush();  //wait for the disk write before reading file
      assertEquals("hi", FileIoUtil.readTextFile(tempFile));
      //the test can only assert that the payload was delivered
      //using breakpoints I think I fixed all deadlocks

      testObject.close();
   }

   @Test
   public void closeTwiceDoesNothing() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileAppender_UT.TempFile.closeTwiceDoesNothing.", ".txt");
      tempFile.deleteOnExit();
      final AsynchronousFileAppender testObject = new AsynchronousFileAppender(tempFile);
      testObject.close();
      testObject.close();
   }

   @Test
   public void appendAfterCloseThrows() throws IOException
   {
      final File tempFile = File.createTempFile("AsynchronousFileAppender_UT.TempFile.appendAfterCloseThrows.", ".txt");
      tempFile.deleteOnExit();
      final AsynchronousFileAppender testObject = new AsynchronousFileAppender(tempFile);
      testObject.close();
      try
      {
         testObject.append("hi");
         fail("Should've thrown");
      }
      catch (final ClosedResourceException actual)
      {
         assertEquals("Can't write to a closed stream", actual.getMessage());
      }
   }
}
