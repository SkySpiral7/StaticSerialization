package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;

public enum ShortSerializableStrategy
{
   ;  //no instances

   public static short read(final EasyReader reader)
   {
      final byte[] data = StreamCorruptedException.throwIfNotEnoughData(reader, 2, "Missing short data");
      final int result = ((data[0] & 0xff) << 8) | (data[1] & 0xff);
      return (short) result;
   }
}
