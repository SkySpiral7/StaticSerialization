package com.github.skySpiral7.java.staticSerialization.stream;

import java.io.Closeable;
import java.nio.charset.Charset;

/**
 * Exists to make an easy API for object serialization use.
 */
public interface EasyReader extends Closeable
{
    void close();
    /**
     * @return true if there are any more bytes
     */
    boolean hasData();
    /**
     * @return the number of bytes in the stream that have not yet been consumed
     */
    int remainingBytes();
    /**
     * Reads bytes from the stream and converts them to a UTF-8 string.
     *
     * @param byteCount the number of bytes (not characters) to read. Be careful not to chop characters in half!
     * @see #readString(int, Charset)
     */
    String readString(final int byteCount);
    /**
     * Reads bytes from the stream and converts them to a string using the given encoding.
     *
     * @param byteCount the number of bytes (not characters) to read. Be careful not to chop characters in half!
     * @param encoding  the character set used to decode the bytes
     * @see #readBytes(int)
     */
    String readString(final int byteCount, final Charset encoding);
    /**
     * Reads a single byte of binary data from the stream.
     *
     * @see #readBytes(int)
     */
    byte readByte();
    /**
     * Reads binary data from the stream.
     *
     * @param byteCount the number of bytes to read
     * @see #readString(int)
     */
    byte[] readBytes(final int byteCount);
}
