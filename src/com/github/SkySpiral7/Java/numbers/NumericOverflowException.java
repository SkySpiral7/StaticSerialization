package com.github.SkySpiral7.Java.numbers;

/**
 * This exception is thrown when a number overflows.
 */
public class NumericOverflowException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a new <code>NumericOverflowException</code>
     * class with an argument indicating the number that overflowed.
     */
    public NumericOverflowException(final Number overflowed) {
        super(overflowed.toString());
    }

    /**
     * Constructs a new <code>NumericOverflowException</code>
     * class with an argument indicating the before and after values of the number.
     */
    public NumericOverflowException(final Number before, final Number after) {
        super("before: "+before+", after: "+after);
    }

    /**
     * Constructs a <code>NumberFormatException</code> with the
     * specified detail message.
     */
    public NumericOverflowException(final String detailMessage) {
        super(detailMessage);
    }

}
