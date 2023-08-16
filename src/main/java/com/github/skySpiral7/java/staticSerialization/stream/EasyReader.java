package com.github.skySpiral7.java.staticSerialization.stream;

import java.io.Closeable;

/**
 * Exists to make an easy API for object serialization use.
 */
public interface EasyReader extends Closeable
{
    void close();
    /**
     * @return true if there are any more bytes
     */
    //TODO: replace with readByte/s taking a string for the exception message
    boolean hasData();
    /**
     * Reads a single byte of binary data from the stream.
     *
     * @see #readBytes(int)
     */
    default byte readByte()
    {
        return readBytes(1)[0];
    }
    /**
     * Reads binary data from the stream.
     *
     * @param byteCount the number of bytes to read
     */
    byte[] readBytes(final int byteCount);
}
