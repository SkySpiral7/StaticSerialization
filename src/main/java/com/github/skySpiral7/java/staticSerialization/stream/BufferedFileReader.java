package com.github.skySpiral7.java.staticSerialization.stream;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;

/**
 * Adapts {@link BufferedInputStream} into {@link EasyReader}
 */
public final class BufferedFileReader implements EasyReader
{
   private final InputStream inputStream;

   /**
    * @param targetFile the file that will be read from
    */
   public BufferedFileReader(final File targetFile)
   {
      if (targetFile.isDirectory())
         throw new IllegalArgumentException("It is not possible to read file contents of a directory");
      //it's ok if the file doesn't exist since writing to it will create it
      if (!targetFile.exists()) throw new IllegalArgumentException("File doesn't exist");

      try
      {
         inputStream = new BufferedInputStream(new FileInputStream(targetFile));
      }
      catch (final FileNotFoundException fileNotFoundException)
      {
         throw new IllegalStateException("Race condition: file was deleted after validation", fileNotFoundException);
      }
   }

   @Override
   public void close()
   {
      try
      {
         inputStream.close();
      }
      catch (final IOException ioException)
      {
         throw new UncheckedIOException(ioException);
      }
   }

   @Override
   public byte[] readBytes(final int requestedByteCount)
   {
      try
      {
         return inputStream.readNBytes(requestedByteCount);
      }
      catch (final IOException ioException)
      {
         throw new UncheckedIOException(ioException);
      }
   }

   @Override
   public byte[] readBytesUntil(final byte finalByte)
   {
      try
      {
         final ByteArrayOutputStream resultBuilder = new ByteArrayOutputStream();
         while (true)
         {
            int byteRead = inputStream.read();
            if (byteRead == -1) break;
            resultBuilder.write(byteRead);
            if (byteRead == finalByte) break;
         }
         return resultBuilder.toByteArray();
      }
      catch (final IOException ioException)
      {
         throw new UncheckedIOException(ioException);
      }
   }
}
