package com.github.skySpiral7.java.staticSerialization.stream;

import java.io.ByteArrayOutputStream;

/**
 * Thread safe.
 */
public class ByteAppender implements EasyAppender
{
    private final ByteArrayOutputStream data = new ByteArrayOutputStream();

    @Override
    public void flush(){}
    @Override
    public void close(){}

    @Override
    public void append(final byte[] newContents)
    {
        data.writeBytes(newContents);
    }

    public byte[] getAllBytes()
    {
        return data.toByteArray();
    }
}
