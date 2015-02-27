package com.github.SkySpiral7.Java.iterators;

import java.util.Iterator;
import java.util.ListIterator;

public interface JumpingIterator<E> extends ListIterator<E> {
	default public void jumpToBeginning(){jumpToBeginning(this);}
	public static <T> void jumpToBeginning(ListIterator<T> iteratorToUse){while(iteratorToUse.hasPrevious()) iteratorToUse.previous();}

	default public void jumpByIndex(int jumpAmount){jumpByIndex(this, jumpAmount);}
	public static <T> void jumpByIndex(ListIterator<T> iteratorToUse, int jumpAmount)
	{
		while(jumpAmount > 0 && iteratorToUse.hasNext()){iteratorToUse.next(); jumpAmount--;}
		while(jumpAmount < 0 && iteratorToUse.hasPrevious()){iteratorToUse.previous(); jumpAmount++;}
	}
	default public void jumpToIndex(int destination){jumpByIndex(this, destination);}
	public static <T> void jumpToIndex(ListIterator<T> iteratorToUse, int destination)
	{
		jumpByIndex(iteratorToUse, (destination - iteratorToUse.nextIndex()));
		//TODO: double check math (likely off by 1). also test jump by
	}

	default public void jumpToEnd(){jumpToEnd(this);}
	public static <T> void jumpToEnd(Iterator<T> iteratorToUse){while(iteratorToUse.hasNext()) iteratorToUse.next();}
}
