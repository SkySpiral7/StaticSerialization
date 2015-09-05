package com.github.SkySpiral7.Java;

/**
 * The list version of ArrayIndexOutOfBoundsException.
 * @see ArrayIndexOutOfBoundsException
 */
public class ListIndexOutOfBoundsException extends IndexOutOfBoundsException {
	private static final long serialVersionUID = 1L;

    /**
     * Constructs a new <code>ListIndexOutOfBoundsException</code>
     * class with an argument indicating the illegal index.
     */
    public ListIndexOutOfBoundsException(final int illegalIndex) {
        super("Index: "+illegalIndex);
    }

    /**
     * Constructs a new <code>ListIndexOutOfBoundsException</code>
     * class with an argument indicating the illegal index and one indicating the size.
     */
    public ListIndexOutOfBoundsException(final int illegalIndex, final int size) {
        super("Index: "+illegalIndex+", Size: "+size);
    }

    /**
     * Constructs an <code>ListIndexOutOfBoundsException</code> class
     * with the specified detail message.
     */
    public ListIndexOutOfBoundsException(final String detailMessage) {
        super(detailMessage);
    }
}
