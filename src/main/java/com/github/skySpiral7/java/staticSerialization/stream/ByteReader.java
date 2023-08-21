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
    public byte[] readBytes(final int requestedByteCount)
    {
        final int remainingBytes = data.length - position;
        final int actualByteLength = Math.min(requestedByteCount, remainingBytes);
        final byte[] result = new byte[actualByteLength];
        //apparently actualByteLength=0 is allowed
        System.arraycopy(data, position, result, 0, actualByteLength);
        position += actualByteLength;
        return result;
    }
}
