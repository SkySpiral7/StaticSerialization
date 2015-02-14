package com.github.SkySpiral7.Java.util;

import java.util.Comparator;
import java.util.Objects;

import com.github.SkySpiral7.Java.pojo.Comparison;

public class ComparableSugar {
	private ComparableSugar(){}

	public static final byte THIS_LESSER = -1;  //aka me first
	public static final byte THIS_EQUAL = 0;
	public static final byte THIS_GREATER = 1;  //aka you first

	public static <T> boolean is(Comparable<T> left, Comparison operation, T right)
		{return isComparisonResult(left.compareTo(right), operation);}

	public static <T> boolean is(T left, Comparison operation, T right, Comparator<T> accordingTo)
		{return isComparisonResult(accordingTo.compare(left, right), operation);}

	public static boolean isComparisonResult(int comparisonResult, Comparison operation)
	{
		Objects.requireNonNull(operation);
		switch (Integer.signum(comparisonResult))
		{
			case 0: return (operation == Comparison.EQUAL_TO || operation == Comparison.GREATER_THAN_OR_EQUAL_TO || operation == Comparison.LESS_THAN_OR_EQUAL_TO);
			case 1: return (operation == Comparison.NOT_EQUAL || operation == Comparison.GREATER_THAN_OR_EQUAL_TO || operation == Comparison.GREATER_THAN);
			//case -1:
			default: return (operation == Comparison.NOT_EQUAL || operation == Comparison.LESS_THAN_OR_EQUAL_TO || operation == Comparison.LESS_THAN);
		}
	}

}
