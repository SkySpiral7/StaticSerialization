package com.github.skySpiral7.java.staticSerialization.stream;

import java.io.Closeable;
import java.io.Flushable;
import java.nio.charset.Charset;

/**
 * Exists to make an easy API for object serialization use.
 */
public interface EasyAppender extends Closeable, Flushable
{
    void flush();
    void close();
    /**
     * Appends a UTF-8 string to the the stream.
     *
     * @see #append(String, Charset)
     */
    void append(final String newContents);
    /**
     * Appends a string to the stream using the given character encoding.
     *
     * @see #append(byte[])
     */
    void append(final String newContents, final Charset encoding);
    /**
     * Appends binary data to the stream.
     *
     * @see #append(String)
     */
    void append(final byte[] newContents);
}
