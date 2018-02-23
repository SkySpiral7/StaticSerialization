package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.fileWrapper.AsynchronousFileReader;

public enum ShortSerializableStrategy
{
   ;  //no instances

   public static short read(final AsynchronousFileReader reader)
   {
      final byte[] data = reader.readBytes(2);
      final int result = ((data[0] & 0xff) << 8) | (data[1] & 0xff);
      return (short) result;
   }
}
