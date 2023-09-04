package com.github.skySpiral7.java.staticSerialization.stream;

import java.io.ByteArrayOutputStream;

/**
 * Simple lightweight stream that collects all bytes. Not thread safe.
 *
 * @see #getAllBytes()
 */
public class ByteAppender implements EasyAppender
{
   private final ByteArrayOutputStream data = new ByteArrayOutputStream();

   /**
    * Does nothing
    */
   @Override
   public void flush(){}

   /**
    * Does nothing
    */
   @Override
   public void close(){}

   @Override
   public void append(final byte[] newContents)
   {
      data.writeBytes(newContents);
   }

   /**
    * Not synchronized with append.
    *
    * @return all bytes that have been written to this stream
    */
   public byte[] getAllBytes()
   {
      return data.toByteArray();
   }
}
