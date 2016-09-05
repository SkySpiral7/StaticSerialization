package com.github.SkySpiral7.Java.util;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Objects;

/**
 * This is a simple utility for reading and writing files.
 * Simple to use but none are efficient.
 *
 * @see #writeToFile(File, String, Charset, boolean)
 * @see #readTextFile(File, Charset)
 */
public enum FileIoUtil
      //I tested this class by hand since a UT would have to duplicate the code
{
   ;  //no instances

   /**
    * @param targetFile
    *       writes to this file (clearing previous content)
    * @param newContents
    *       the file will contain only this UTF-8 string
    * @throws RuntimeException
    *       of FileNotFoundException or IOException
    * @see #writeToFile(File, String, Charset, boolean)
    */
   public static void writeToFile(final File targetFile, final String newContents)
   {
      writeToFile(targetFile, newContents, StandardCharsets.UTF_8, false);
   }

   /**
    * @param targetFile
    *       writes to this file (keeping previous content)
    * @param newContents
    *       the file will gain this UTF-8 string (after current contents)
    * @throws RuntimeException
    *       of FileNotFoundException or IOException
    * @see #writeToFile(File, String, Charset, boolean)
    */
   public static void appendToFile(final File targetFile, final String newContents)
   {
      writeToFile(targetFile, newContents, StandardCharsets.UTF_8, true);
   }

   /**
    * This method opens the file, writes (which may create it), then closes the file.
    * Therefore it is inefficient to call this more than once when appending.
    *
    * @param targetFile
    *       writes to this file
    * @param newContents
    *       the string contents to be written
    * @param encoding
    *       the character encoding to write to the file in
    * @param willAppend
    *       true if the current file contents should be kept
    * @throws RuntimeException
    *       of FileNotFoundException or IOException
    */
   public static void writeToFile(final File targetFile, final String newContents, final Charset encoding,
         final boolean willAppend)
   {
      if (targetFile.isDirectory()) throw new IllegalArgumentException("It is not possible to write to a directory");
      Objects.requireNonNull(newContents);

      try (final Writer writer = new BufferedWriter(new OutputStreamWriter(
            new FileOutputStream(targetFile, willAppend), encoding));)
      {
         // might create the file
         writer.write(newContents);
      }
      catch (final IOException e)
      {
         throw new RuntimeException(e);
      }
   }

   /**
    * This method opens the file, writes (which may create it), then closes the file.
    * Therefore it is inefficient to call this more than once when appending.
    *
    * @param targetFile
    *       writes to this file
    * @param newContents
    *       the binary contents to be written
    * @throws RuntimeException
    *       of FileNotFoundException or IOException
    */
   public static void writeToFile(final File targetFile, final byte[] newContents, final boolean willAppend)
   {
      if (targetFile.isDirectory()) throw new IllegalArgumentException("It is not possible to write to a directory");
      Objects.requireNonNull(newContents);

      try (final OutputStream writer = new BufferedOutputStream(new FileOutputStream(targetFile, willAppend));)
      {
         // might create the file
         writer.write(newContents);
      }
      catch (final IOException e)
      {
         throw new RuntimeException(e);
      }
   }

   /**
    * @param targetFile
    *       the UTF-8 file to be read
    * @return the entire file contents
    *
    * @throws RuntimeException
    *       of FileNotFoundException or IOException
    * @throws IllegalArgumentException
    *       if the file is larger than a string can hold
    * @see #readTextFile(File, Charset)
    */
   public static String readTextFile(final File targetFile)
   {
      return readTextFile(targetFile, StandardCharsets.UTF_8);
   }

   /**
    * This method reads the entire file and loads it into a string.
    * This is obviously bad for performance.
    *
    * @param targetFile
    *       the file to be read
    * @param encoding
    *       the character encoding to read the file with
    * @return the entire file contents
    *
    * @throws RuntimeException
    *       of FileNotFoundException or IOException
    * @throws IllegalArgumentException
    *       if the file is a directory or if the file doesn't exist
    * @throws IllegalArgumentException
    *       if the file is larger than a string can hold. The size is estimated however this method shouldn't be
    *       used for files anywhere near the limit.
    */
   public static String readTextFile(final File targetFile, final Charset encoding)
   {
      if (targetFile.isDirectory()) throw new IllegalArgumentException(
            "It is not possible to read file contents of a directory");
      if (!targetFile.exists()) throw new IllegalArgumentException("File doesn't exist");

      //this is only an estimate since character size is not always 1 byte
      if (targetFile.length() > Integer.MAX_VALUE) throw new IllegalArgumentException(
            "File (length " + targetFile.length() + ") too large to fit into a string");
      //for completeness I could count the number of characters read and throw but it's better to have this hedge

      final StringBuilder returnValue = new StringBuilder();
      try (final Reader reader = new BufferedReader(new InputStreamReader(new FileInputStream(
            targetFile.getAbsolutePath()), encoding));)
      {
         while (true)
         {
            final int charRead = reader.read();
            if (-1 == charRead) break;  // if end of file
            returnValue.append((char) charRead);
         }
      }
      catch (final Exception e)
      {
         throw new RuntimeException(e);
      }
      return returnValue.toString();
   }

   /**
    * This method reads the entire file and loads it into a byte array.
    *
    * @param targetFile
    *       the file to be read
    * @return the entire file contents
    *
    * @throws RuntimeException
    *       of FileNotFoundException or IOException
    * @throws IllegalArgumentException
    *       if the file is a directory, if the file doesn't exist, or if the file is larger than a byte array can
    *       hold
    * @throws IllegalStateException
    *       if the file has fewer bytes than the length indicated. This is possible if another thread is changing
    *       the file
    */
   public static byte[] readBinaryFile(final File targetFile)
   {
      if (targetFile.isDirectory()) throw new IllegalArgumentException(
            "It is not possible to read file contents of a directory");
      if (!targetFile.exists()) throw new IllegalArgumentException("File doesn't exist");

      if (targetFile.length() > Integer.MAX_VALUE) throw new IllegalArgumentException(
            "File (length " + targetFile.length() + ") too large to fit into a byte[]");

      final byte[] result = new byte[(int) targetFile.length()];
      try
      {
         try (final InputStream input = new BufferedInputStream(new FileInputStream(targetFile));)
         {
            int totalBytesRead = 0;
            while (totalBytesRead < result.length)
            {
               final int bytesRemaining = result.length - totalBytesRead;
               final int bytesRead = input.read(result, totalBytesRead, bytesRemaining);
               if (bytesRead == -1) throw new IllegalStateException(
                     "file contains fewer bytes then its length indicated");
               totalBytesRead += bytesRead;
               //this loop usually has a single iteration
            }
         }
      }
      catch (final IOException ex)
      {
         throw new RuntimeException(ex);
      }
      return result;
   }
}
