package com.github.skySpiral7.java.staticSerialization.fileWrapper;

import java.io.Closeable;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.Flushable;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.TimeUnit;

import com.github.skySpiral7.java.staticSerialization.exception.ClosedResourceException;
import com.github.skySpiral7.java.util.FileIoUtil;

/**
 * <p>This class lets you send data into a queue that another thread will write to the file.
 * Therefore the other thread will be waiting on the disk instead of the main thread.
 * The main thread can build up a queue, do other stuff, and hopefully not need to wait when calling close.
 * Note that calling append then immediately calling close will force you to wait for the disk and
 * thus using this class was pointless.</p>
 * <p><b>Warning:</b> this is the first Java class I've made with multiple threads.</p>
 *
 * @see #close()
 * @see #append(byte[])
 */
public final class AsynchronousFileAppender implements Closeable, Flushable
{
   private final WriterClass writer;
   private boolean amOpen = true;

   /**
    * Note that this class only appends to the file (not write). If appending is not desired then you must
    * clear the file yourself beforehand.
    *
    * @param targetFile the file that will be appended to
    *
    * @see FileIoUtil#writeToFile(File, String)
    * use FileIoUtil.writeToFile(targetFile, "") to clear the file
    */
   public AsynchronousFileAppender(final File targetFile)
   {
      //TODO: I don't know if these classes do anything beyond BufferedInputStream etc
      if (targetFile.isDirectory()) throw new IllegalArgumentException("It is not possible to write to a directory");
      //it's ok if the file doesn't exist since writing to it will create it

      try
      {
         writer = new WriterClass(new FileOutputStream(targetFile, true), this);
      }
      catch (final FileNotFoundException e)
      {
         throw new RuntimeException(e);
      }
      new Thread(writer).start();
   }

   /**
    * Waits until the entire current queue is written to the file.
    * If the queue is already empty then this method does nothing.
    */
   @Override
   public void flush()
   {
      while (!writer.queue.isEmpty())
      {
         try
         {
            Thread.sleep(10);
         }
         catch (final InterruptedException e)
         {
            throw new RuntimeException(e);
         }
      }
   }

   /**
    * Flushes (which might cause waiting) then closes the file stream.
    *
    * @see #flush()
    */
   @Override
   public void close()
   {
      if (!amOpen) return;  //this is to prevent getting stuck by this.wait() below
      this.flush();  //need to finish writing to disk before telling it to stop writing

      try
      {
         synchronized (this)
         {
            writer.shouldWrite = false;
            this.wait();  //wait for the writer's thread to stop before closing
            //therefore all resources will be released when this method returns
            //as opposed to closing the file stream and assuming the other thread will stop
         }
         writer.outputStream.close();
      }
      catch (final IOException | InterruptedException e)
      {
         throw new RuntimeException(e);
      }
      amOpen = false;
   }

   /**
    * Appends a UTF-8 string to the file.
    * This method will wait if the queue is full.
    *
    * @see #append(String, Charset)
    */
   public void append(final String newContents)
   {
      append(newContents.getBytes(StandardCharsets.UTF_8));
   }

   /**
    * Appends a string to the file using the given character encoding.
    * This method will wait if the queue is full.
    *
    * @see #append(byte[])
    */
   public void append(final String newContents, final Charset encoding)
   {
      append(newContents.getBytes(encoding));
   }

   /**
    * Appends binary data to the file.
    * This method will wait if the queue is full.
    *
    * @see #append(String)
    */
   public void append(final byte[] newContents)
   {
      if (!amOpen) throw new ClosedResourceException("Can't write to a closed stream");
      //otherwise it would get put in a queue that would never be emptied

      try
      {
         for (int i = 0; i < newContents.length; i++)
         {
            //not sure if this is better than converting to Byte[] then addAll
            writer.queue.put(newContents[i]);
         }
      }
      catch (InterruptedException e)
      {
         throw new RuntimeException(e);
      }
   }

   private final static class WriterClass implements Runnable
   {
      public final OutputStream outputStream;
      /**
       * I don't know enough about concurrency to make a simple disruptor.
       * And the fact that each queue entry holds 1 byte makes me think I could be doing this better.
       */
      public final ArrayBlockingQueue<Byte> queue = new ArrayBlockingQueue<>(1024000);
      /**
       * I'm using Object.wait ect because I don't know how to use java.util.concurrent.locks.Lock
       */
      private final Object notifyMeWhenDone;
      public volatile boolean shouldWrite = true;

      public WriterClass(final OutputStream outputStream, final Object notifyMeWhenDone)
      {
         this.outputStream = outputStream;
         this.notifyMeWhenDone = notifyMeWhenDone;
      }

      @Override
      public void run()
      {
         while (shouldWrite)
         {
            try
            {
               //the timeout exists (instead of take) so that shouldWrite can be examined again
               final Byte data = queue.poll(10, TimeUnit.MILLISECONDS);
               if (data != null)
               {
                  //since primitive bytes were added, this can only be null if there was a timeout
                  outputStream.write(data);
               }
            }
            catch (final InterruptedException | IOException e)
            {
               throw new RuntimeException(e);
            }
         }
         synchronized (notifyMeWhenDone)
         {
            //notify the main thread that I am done writing to disk
            notifyMeWhenDone.notify();
         }
      }
   }
}
