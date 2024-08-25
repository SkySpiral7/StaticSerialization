package com.github.skySpiral7.java.staticSerialization.stream;

import java.io.ByteArrayOutputStream;

/**
 * Simple lightweight stream that reads a given byte array. Not thread safe.
 */
public class ByteReader implements EasyReader
{
   private final byte[] data;
   /**
    * The array index of the next byte to read.
    * Will be data.length when done.
    */
   private int position;

   /**
    * @param data all data contained in this stream
    */
   public ByteReader(byte[] data)
   {
      this.data = data;
      position = 0;
   }

   /**
    * Does nothing
    */
   @Override
   public void close(){}

   /**
    * Returns a section of the stream's data. Not thread safe.
    *
    * @param requestedByteCount the maximum number of bytes to read
    * @see EasyReader#readBytes(int)
    */
   @Override
   public byte[] readBytes(final int requestedByteCount)
   {
      final int remainingBytes = data.length - position;
      final int actualByteLength = Math.min(requestedByteCount, remainingBytes);
      final byte[] result = new byte[actualByteLength];
      //apparently actualByteLength=0 is allowed
      System.arraycopy(data, position, result, 0, actualByteLength);
      position += actualByteLength;
      return result;
   }

   @Override
   public byte[] readBytesUntil(byte finalByte)
   {
      final ByteArrayOutputStream resultBuilder = new ByteArrayOutputStream(data.length);

      int newPosition = position;
      for (; newPosition < data.length; newPosition++)
      {
         if (data[newPosition] == finalByte)
         {
            //newPosition will be set to the byte after the match
            newPosition++;
            break;
         }
      }
      //no match (including empty) will have newPosition set to length

      //apparently len=0 is allowed
      resultBuilder.write(data, position, (newPosition - position));
      position = newPosition;
      return resultBuilder.toByteArray();
   }
}
