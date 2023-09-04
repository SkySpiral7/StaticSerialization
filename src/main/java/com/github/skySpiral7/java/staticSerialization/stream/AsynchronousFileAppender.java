package com.github.skySpiral7.java.staticSerialization.stream;

import com.github.skySpiral7.java.staticSerialization.exception.ClosedResourceException;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * <p>This class lets you send data into a queue that another thread will write to the file.
 * Therefore the other thread will be waiting on the disk instead of the main thread. The main thread can build up a queue, do other stuff,
 * and hopefully not need to wait when calling close. Note that calling append then immediately calling close will force you to wait for the
 * disk and thus using this class was pointless.</p>
 * <p><b>Warning:</b> this is the first Java class I've made with multiple threads.</p>
 *
 * @see #close()
 * @see #append(byte[])
 */
public final class AsynchronousFileAppender implements EasyAppender
{
   private final WriterClass writer;
   private boolean amOpen = true;

   /**
    * Note that this class clears the file contents then only appends.
    *
    * @param targetFile the file that will be appended to
    */
   public AsynchronousFileAppender(final File targetFile)
   {
      //TODO: I don't know if these classes do anything beyond BufferedInputStream etc
      //mine has better API though
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
         writer = new WriterClass(new FileOutputStream(targetFile, true), this);
      }
      catch (final FileNotFoundException e)
      {
         throw new RuntimeException(e);
      }
      new Thread(writer).start();
   }

   /**
    * Waits until the entire current queue is written to the file. If the queue is already empty then this method does nothing.
    */
   @Override
   public void flush()
   {
      while (!writer.queue.isEmpty())
      {
         try
         {
            //TODO: could update to sleep less see https://stackoverflow.com/a/54394101
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
      if (!amOpen) return;  //fast check
      this.flush();  //need to finish writing to disk before telling it to stop writing

      try
      {
         synchronized (this)
         {
            if (!amOpen) return;  //check again inside the lock
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
    * Appends binary data to the file. This method will wait if the queue is full.
    */
   @Override
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
       * I don't know enough about concurrency to make a simple disruptor. And the fact that each queue entry holds 1 byte makes me think I
       * could be doing this better.
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
