package com.github.skySpiral7.java.staticSerialization.stream;

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
    public byte readByte()
    {
        position++;
        return data[position - 1];
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