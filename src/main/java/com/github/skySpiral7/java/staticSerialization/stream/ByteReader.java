package com.github.skySpiral7.java.staticSerialization.stream;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 * Not thread safe.
 */
public class ByteReader implements EasyReader {
    private final byte[] data;
    /**
     * The array index of the next byte to read.
     * Will be data.length when done.
     */
    private int position;

    public ByteReader(byte[] data)
    {
        this.data = data;
        position = 0;
    }

    @Override
    public void close(){}

    @Override
    public boolean hasData()
    {
        return position < data.length;
    }

    @Override
    public int remainingBytes()
    {
        return data.length - position;
    }

    @Override
    public String readString(int byteCount)
    {
        return readString(byteCount, StandardCharsets.UTF_8);
    }

    @Override
    public String readString(int byteCount, Charset encoding)
    {
        return new String(readBytes(byteCount), encoding);
    }

    @Override
    public byte readByte()
    {
        return readBytes(1)[0];
    }

    @Override
    public byte[] readBytes(int byteCount)
    {
        final byte[] result = new byte[byteCount];
        System.arraycopy(data, position, result, 0, byteCount);
        position += byteCount;
        return result;
    }
}
