package com.github.SkySpiral7.Java.dataStructures;

import java.util.List;

public class Range
{
	private Boundary lower;
	private Boundary upper;

	public Range(Boundary lower, Boundary upper){}
	public Range(Number lower, String rangePattern, Number upper){}

	public static Boundary exclusive(Number num){return null;}
	public static Boundary inclusive(Number num){return null;}

	//TODO: use <T extends Number & Comparable> instead
	public boolean contains(Number testNum){return false;}

	public Object createArray(Object typeOf){return createArray(typeOf, lower.getIncrementAmonut());}
	public Object createArray(Object typeOf, Number stepBy){return null;}
	public List<?> createList(Class<?> typeOf){return createList(typeOf, lower.getIncrementAmonut());}
	public List<?> createList(Class<?> typeOf, Number stepBy){return null;}

	//TODO: make different Boundary classes for long, double, and Number
	private static class Boundary
	{
		private final Number value;
		public Boundary(Number value){this.value = value;}

		public boolean isInclusive(){return false;}
		public Number getValue(){return value;}
		public Number getIncrementAmonut(){return null;}
		public Number add(Number starting, Number stepBy){return null;}
	}
}
