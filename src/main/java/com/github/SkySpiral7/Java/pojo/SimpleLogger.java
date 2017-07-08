package com.github.SkySpiral7.Java.pojo;

import java.io.BufferedOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Objects;

/**
 * <p>This class is a very simple logger. It is useful for quick and dirty debugging.
 * For example if you need to debug a loop that will iterate 10,000 times but you
 * don't want to pollute the log file normally used then you can use this for a quick
 * temporary log. Or maybe in a loop if you want to track multiple things you can put
 * them each in their own log so you can see how they change over time without having
 * to parse which line belongs to which variable.</p>
 *
 * <p>This class is final to make it perform faster. This class should not gain functionality
 * and should not be a method parameter or class field. This is because this class is intended for quick debugging
 * and should not be used in production (or other environments like QA) for that see a real logger like Log4J 2.</p>
 *
 * <p>Note that all exceptions thrown by this class are unchecked in order to simplify the intended use case.</p>
 */
public final class SimpleLogger implements Closeable
{
   private final OutputStream writer;

   /**
    * @param file the File that will be written to (even if it does not exist).
    *
    * @throws IllegalArgumentException if the File is a directory (which can't be written to)
    * @throws RuntimeException         of FileNotFoundException from the constructor of FileOutputStream
    * @see FileOutputStream#FileOutputStream(File, boolean)
    */
   public SimpleLogger(final File file)
   {
      if (file.isDirectory()) throw new IllegalArgumentException("It is not possible to log to a directory");
      try
      {
         writer = new BufferedOutputStream(new FileOutputStream(file, false));
      }
      catch (final FileNotFoundException e)
      {
         throw new RuntimeException(e);
      }
   }

   /**
    * Append to the log. Note that an end line is not added.
    * The log will be created if it does not exist.
    *
    * @param text the text to be appended exactly as passed in
    *
    * @throws RuntimeException of IOException from OutputStream.write
    * @see OutputStream#write(byte[])
    */
   public void append(final String text)
   {
      Objects.requireNonNull(text);
      try
      {
         writer.write(text.getBytes(StandardCharsets.UTF_8));
      }
      catch (final IOException e)
      {
         throw new RuntimeException(e);
      }
   }

   /**
    * Append to the log. An end line is appended to the string.
    * The log will be created if it does not exist.
    *
    * @param line the text to be appended followed by a system end line
    *
    * @throws RuntimeException of IOException from OutputStream.write
    * @see OutputStream#write(byte[])
    */
   public void appendLine(final String line)
   {
      Objects.requireNonNull(line);
      this.append(line + System.lineSeparator());
   }

   /**
    * @throws RuntimeException of IOException from OutputStream.close
    * @see OutputStream#close()
    */
   @Override
   public void close()
   {
      try
      {
         writer.close();
      }
      catch (final IOException e)
      {
         throw new RuntimeException(e);
      }
   }

}
