package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;

public class ShortSerializableStrategy
{
   private final EasyReader reader;

   public ShortSerializableStrategy(EasyReader reader){this.reader = reader;}

   public short read(String corruptMessage)
   {
      final byte[] data = StreamCorruptedException.throwIfNotEnoughData(reader, 2, corruptMessage);
      final int result = ((data[0] & 0xff) << 8) | (data[1] & 0xff);
      return (short) result;
   }
}
