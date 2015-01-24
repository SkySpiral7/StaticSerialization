package com.github.SkySpiral7.Java.dataStructures.sameObject;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import com.github.SkySpiral7.Java.util.JsonHelper;

/**
 * @deprecated use IdentityHashSet instead
 * @see IdentityHashSet
 */
public class SameObjectSet<E> extends SameObjectList<E> implements Set<E>
{
	private static final long serialVersionUID = 1L;

	/**
	 * The no-arg construct creates an empty set.
	 */
	public SameObjectSet(){super();}
	public SameObjectSet(int initialCapacity){super(initialCapacity);}
	public SameObjectSet(Collection<? extends E> initialElements){this(initialElements.size()); this.addAll(initialElements);}
	public SameObjectSet(E[] initialElements){this(Arrays.asList(initialElements));}

	@Override
	public boolean containsAll(Collection<?> collectionToSearch) {
		Objects.requireNonNull(collectionToSearch);
		return super.containsAll(collectionToSearch);
	}

	@Override
	public <T> T[] toArray(T[] destinationArray) {
		Objects.requireNonNull(destinationArray);
		return super.toArray(destinationArray);
	}

	@Override
	public boolean add(E newElement) {
		if(this.contains(newElement)) return false;
		return super.add(newElement);  //always returns true
	}

	@Override
	public boolean addAll(Collection<? extends E> collectionToAdd) {
		boolean hasChanged = false;
		for(E element : collectionToAdd)
		{
			hasChanged = this.add(element) || hasChanged;  //must be in this order to avoid short circuit
		}
		return hasChanged;
	}

	@Override
	public boolean removeAll(Collection<?> collectionToRemove) {
		Objects.requireNonNull(collectionToRemove);
		return super.removeAll(collectionToRemove);
	}

	@Override
	public boolean retainAll(Collection<?> collectionToRetain) {
		Objects.requireNonNull(collectionToRetain);
		return super.retainAll(collectionToRetain);
	}

	@Override
	public boolean equals(Object obj) {
		if(obj == this) return true;
		if(obj instanceof List<?>) return super.equals(obj);
		if(!(obj instanceof Set<?>)) return false;

		Set<?> otherList = (Set<?>) obj;
		if(this.size() != otherList.size()) return false;
		return this.containsAll(otherList);
	}

	@Override
	public String toString() {
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append(JsonHelper.toStringHeader(this));
		stringBuilder.append('{');
		for(int index = 0; index < this.size(); index++)
		{
			stringBuilder.append(JsonHelper.stringify(this.get(index)));
			stringBuilder.append(": true");
			if(index+1 < this.size()) stringBuilder.append(", ");
		}
		stringBuilder.append("}}");
		return stringBuilder.toString();
	}

}
