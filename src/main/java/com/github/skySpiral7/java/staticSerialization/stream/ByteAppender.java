package com.github.skySpiral7.java.staticSerialization.stream;

import java.io.ByteArrayOutputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 * Not thread safe.
 */
public class ByteAppender implements EasyAppender
{
    private final ByteArrayOutputStream data = new ByteArrayOutputStream();

    @Override
    public void flush(){}
    @Override
    public void close(){}

    @Override
    public void append(final String newContents)
    {
        append(newContents.getBytes(StandardCharsets.UTF_8));
    }

    @Override
    public void append(final String newContents, final Charset encoding)
    {
        append(newContents.getBytes(encoding));
    }

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
