package com.github.SkySpiral7.Java.iterators;

import java.util.ListIterator;

public interface JumpingIterator<E> extends ListIterator<E> {
	default public void jumpToBeginning(){while(this.hasPrevious()) this.previous();}
	default public void jump(int jumpAmount)
	{
		while(jumpAmount > 0 && this.hasNext()){this.next(); jumpAmount--;}
		while(jumpAmount < 0 && this.hasPrevious()){this.previous(); jumpAmount++;}
	}
	default public void jumpToEnd(){while(this.hasNext()) this.next();}
}
