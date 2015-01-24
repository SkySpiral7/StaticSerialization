package com.github.SkySpiral7.Java.pojo;

import java.util.Objects;

//a / n becomes a = (n * wholeResult) + remainder. therefore remainder >= 0
//if a = 0 then remainder = wholeResult = 0
//if n = 0 then remainder = a and wholeResult = 0 (although any number would be correct)
public class IntegerQuotient<T extends Number> {
	protected T wholeResult;
	protected T remainder;

    public IntegerQuotient(T wholeResult, T remainder) {
		Objects.requireNonNull(wholeResult);
		Objects.requireNonNull(remainder);
    	this.wholeResult = wholeResult;
		this.remainder = remainder;
	}

    /**
     * @return the whole integer that resulted from the division. It may be zero but not null.
     */
	public T getWholeResult(){return wholeResult;}
    /**
     * @return the integer that remained after the division. It may be zero but not null or negative.
     */
	public T getRemainder(){return remainder;}
}
