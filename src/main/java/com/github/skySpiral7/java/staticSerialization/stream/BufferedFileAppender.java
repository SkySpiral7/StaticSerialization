package com.github.skySpiral7.java.staticSerialization.stream;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;

/**
 * Adapts {@link BufferedOutputStream} into {@link EasyAppender}
 */
public final class BufferedFileAppender implements EasyAppender
{
   private final OutputStream outputStream;

   /**
    * Note that this class clears the file contents then only appends.
    *
    * @param targetFile the file that will be appended to
    */
   public BufferedFileAppender(final File targetFile)
   {
      if (targetFile.isDirectory())
         throw new IllegalArgumentException("It is not possible to write to a directory (" + targetFile + ")");
      //it's ok if the file doesn't exist since writing to it will create it

      //start by clearing the file so that all writes can append (also this is fail fast to prove that writing is possible)
      try
      {
         Files.write(targetFile.toPath(), new byte[0]);
      }
      catch (IOException ioException)
      {
         throw new UncheckedIOException(ioException);
      }

      try
      {
         outputStream = new BufferedOutputStream(new FileOutputStream(targetFile, true));
      }
      catch (final FileNotFoundException e)
      {
         throw new RuntimeException(e);
      }
   }

   @Override
   public void flush()
   {
      try
      {
         outputStream.flush();
      }
      catch (final IOException ioException)
      {
         throw new UncheckedIOException(ioException);
      }
   }

   @Override
   public void close()
   {
      try
      {
         outputStream.close();
      }
      catch (final IOException ioException)
      {
         throw new UncheckedIOException(ioException);
      }
   }

   @Override
   public void append(final byte[] newContents)
   {
      try
      {
         outputStream.write(newContents);
      }
      catch (final IOException ioException)
      {
         throw new UncheckedIOException(ioException);
      }
   }
}
