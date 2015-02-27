package com.github.SkySpiral7.Java.iterators;

import com.github.SkySpiral7.Java.dataStructures.ModCountList;

public class JumpingIteratorExternalRandomAccess<E> extends ListIteratorExternal<E> implements JumpingIterator<E> {

	public JumpingIteratorExternalRandomAccess(ModCountList<E> underlyingList, int initialIndex){super(underlyingList, initialIndex);}
	public JumpingIteratorExternalRandomAccess(ModCountList<E> underlyingList){super(underlyingList);}

	@Override
	public void jumpToBeginning(){cursor = 0;}
	@Override
	public void jumpByIndex(int jumpAmount)
	{
		cursor += jumpAmount;
		if(cursor < 0) cursor = 0;
		if(cursor > underlyingList.size()) cursor = underlyingList.size();
	}
	@Override
	public void jumpToIndex(int destination)
	{
		//overriding this method doesn't improve performance over the default
		cursor = destination;
		if(cursor < 0) cursor = 0;
		if(cursor > underlyingList.size()) cursor = underlyingList.size();
	}
	@Override
	public void jumpToEnd(){cursor = underlyingList.size();}

}
