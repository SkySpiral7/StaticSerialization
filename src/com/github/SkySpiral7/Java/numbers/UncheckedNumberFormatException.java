package com.github.SkySpiral7.Java.numbers;

/**
 * Same as the JRE version except unchecked.
 *
 * @see java.lang.NumberFormatException
 */
public class UncheckedNumberFormatException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a <code>NumberFormatException</code> with the
     * specified detail message.
     */
    public UncheckedNumberFormatException(final String detailMessage) {
        super(detailMessage);
    }

    /**
     * Factory method for making a <code>NumberFormatException</code>
     * given the specified input which caused the error.
     *
     * @param invalidString the input causing the error
     */
    public static UncheckedNumberFormatException forInputString(final String invalidString) {
        return new UncheckedNumberFormatException("input string: \"" + invalidString + "\"");
    }
    /**
     * Factory method for making a <code>NumberFormatException</code>
     * given the specified input which caused the error.
     *
     * @param invalidString the input causing the error
     */
    public static UncheckedNumberFormatException forInputString(final String invalidString, final int radix) {
        return new UncheckedNumberFormatException("radix: "+radix+" input string: \"" + invalidString + "\"");
    }
}
