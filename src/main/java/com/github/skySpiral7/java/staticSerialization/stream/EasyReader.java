package com.github.skySpiral7.java.staticSerialization.stream;

import com.github.skySpiral7.java.staticSerialization.exception.ClosedResourceException;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;

import java.io.Closeable;

/**
 * Exists to make an easy API for object serialization use.
 * By design, you can't ask how many bytes remain since those bytes
 * might belong to another class and would thus be misleading.
 */
public interface EasyReader extends Closeable
{
   void close();

   /**
    * Reads binary data from the stream. The returned array's length will be requestedByteCount unless
    * the stream doesn't contain enough bytes in which case the returned array length will be however
    * many bytes remain (possibly 0).
    *
    * @param requestedByteCount the maximum number of bytes to read
    * @throws ClosedResourceException if the stream is closed
    * @see StreamCorruptedException#throwIfNotEnoughData(EasyReader, int, String)
    */
   byte[] readBytes(final int requestedByteCount);

   /**
    * Reads binary data from the stream until the expected byte is found. If found, the returned array's final byte will
    * match the param. If the byte isn't found then the result will be all remaining bytes (possibly 0).
    *
    * @param finalByte stop when this byte is found
    * @throws ClosedResourceException if the stream is closed
    * @see StreamCorruptedException#throwIfNotByteTerminated(EasyReader, byte, String)
    */
   byte[] readBytesUntil(final byte finalByte);
}
