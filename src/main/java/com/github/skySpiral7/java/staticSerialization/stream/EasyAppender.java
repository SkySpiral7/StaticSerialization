package com.github.skySpiral7.java.staticSerialization.stream;

import java.io.Closeable;
import java.io.Flushable;
import java.nio.charset.StandardCharsets;

/**
 * Exists to make an easy API for object serialization use.
 */
public interface EasyAppender extends Closeable, Flushable
{
    void flush();
    void close();

    /**
     * Appends a UTF-8 string to the stream.
     *
     * @see #append(byte[])
     */
    default void append(final String newContents)
    {
        //this is only called by tests
        append(newContents.getBytes(StandardCharsets.UTF_8));
    }
    /**
     * Appends a single byte to the stream.
     *
     * @see #append(byte[])
     */
    default void append(final byte data)
    {
        //only used once but fair enough to exist as default
        append(new byte[]{data});
    }

    /**
     * Appends binary data to the stream.
     *
     * @see #append(String)
     */
    void append(final byte[] newContents);
}
