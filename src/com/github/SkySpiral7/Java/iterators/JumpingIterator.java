package com.github.SkySpiral7.Java.iterators;

import java.util.Iterator;
import java.util.ListIterator;

public interface JumpingIterator<E> extends ListIterator<E> {
	default public void jumpToBeginning(){jumpToBeginning(this);}
	public static <T> void jumpToBeginning(ListIterator<T> iteratorToUse){while(iteratorToUse.hasPrevious()) iteratorToUse.previous();}

	default public void jump(int jumpAmount){jump(this, jumpAmount);}
	public static <T> void jump(ListIterator<T> iteratorToUse, int jumpAmount)
	{
		while(jumpAmount > 0 && iteratorToUse.hasNext()){iteratorToUse.next(); jumpAmount--;}
		while(jumpAmount < 0 && iteratorToUse.hasPrevious()){iteratorToUse.previous(); jumpAmount++;}
	}

	default public void jumpToEnd(){jumpToEnd(this);}
	public static <T> void jumpToEnd(Iterator<T> iteratorToUse){while(iteratorToUse.hasNext()) iteratorToUse.next();}
}
