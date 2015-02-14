package com.github.SkySpiral7.Java;

public class ListIndexOutOfBoundsException extends IndexOutOfBoundsException {
	private static final long serialVersionUID = 1L;

	/**
     * Constructs an <code>ListIndexOutOfBoundsException</code> with no
     * detail message.
     */
    public ListIndexOutOfBoundsException() {
        super();
    }

    /**
     * Constructs a new <code>ListIndexOutOfBoundsException</code>
     * class with an argument indicating the illegal index.
     */
    public ListIndexOutOfBoundsException(int illegalIndex) {
        super("Index: "+illegalIndex);
    }

    /**
     * Constructs a new <code>ListIndexOutOfBoundsException</code>
     * class with an argument indicating the illegal index and one indicating the size.
     */
    public ListIndexOutOfBoundsException(int illegalIndex, int size) {
        super("Index: "+illegalIndex+", Size: "+size);
    }

    /**
     * Constructs an <code>ListIndexOutOfBoundsException</code> class
     * with the specified detail message.
     */
    public ListIndexOutOfBoundsException(String detailMessage) {
        super(detailMessage);
    }
}
