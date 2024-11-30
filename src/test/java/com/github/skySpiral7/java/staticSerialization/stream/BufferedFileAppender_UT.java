package com.github.skySpiral7.java.staticSerialization.stream;

import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class BufferedFileAppender_UT
{
   @Test
   public void constructor_clears() throws IOException
   {
      final File tempFile = File.createTempFile("BufferedFileAppender_UT.TempFile.constructor_clears.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "test");
      new BufferedFileAppender(tempFile).close();
      assertEquals("", FileIoUtil.readTextFile(tempFile));
   }

   @Test
   public void writesToFile() throws IOException
   {
      final File tempFile = File.createTempFile("BufferedFileAppender_UT.TempFile.writesToFile.", ".txt");
      tempFile.deleteOnExit();
      final BufferedFileAppender testObject = new BufferedFileAppender(tempFile);

      testObject.append("hi");
      testObject.flush();  //wait for the disk write before reading file
      assertEquals("hi", FileIoUtil.readTextFile(tempFile));

      testObject.close();
   }

   @Test
   public void closeTwiceDoesNothing() throws IOException
   {
      final File tempFile = File.createTempFile("BufferedFileAppender_UT.TempFile.closeTwiceDoesNothing.", ".txt");
      tempFile.deleteOnExit();
      final BufferedFileAppender testObject = new BufferedFileAppender(tempFile);
      testObject.close();
      testObject.close();
   }

   @Test
   public void appendAfterCloseThrows() throws IOException
   {
      final File tempFile = File.createTempFile("BufferedFileAppender_UT.TempFile.appendAfterCloseThrows.", ".txt");
      tempFile.deleteOnExit();
      final BufferedFileAppender testObject = new BufferedFileAppender(tempFile);
      testObject.close();

      assertThrows(UncheckedIOException.class, () -> {
         testObject.append("hi");
         testObject.flush();
      });
   }
}
