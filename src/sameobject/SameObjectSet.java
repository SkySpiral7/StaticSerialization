package src.sameobject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

//TODO: add javadoc. note that it violates some of the interface because it does not use element.equals
public class SameObjectSet<E> implements Set<E>
{
	private SameObjectList<E> dataList;

	/**
	 * The no-arg construct creates an empty set.
	 */
	public SameObjectSet(){dataList = new SameObjectList<E>();}
	public SameObjectSet(Collection<? extends E> initialElements){this(); this.addAll(initialElements);}
	public SameObjectSet(E[] initialElements){this(Arrays.asList(initialElements));}

	@Override
	public int size() {
		return dataList.size();
	}

	@Override
	public boolean isEmpty() {
		return this.size() == 0;
	}

	@Override
	public boolean contains(Object objectToFind) {
		return dataList.contains(objectToFind);
	}

	@Override
	public boolean containsAll(Collection<?> collectionToSearch) {
		for(Object element : collectionToSearch)
		{
			if(!this.contains(element)) return false;
		}
		return true;
	}

	@Override
	public Iterator<E> iterator() {
		return dataList.iterator();
	}

	@Override
	public Object[] toArray() {
		return dataList.toArray();
	}

	@Override
	public <T> T[] toArray(T[] destinationArray) {
		return dataList.toArray(destinationArray);
	}

	@Override
	public boolean add(E newElement) {
		if(this.contains(newElement)) return false;
		return dataList.add(newElement);  //always returns true
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
	public boolean remove(Object objectToRemove) {
		return dataList.remove(objectToRemove);
	}

	@Override
	public boolean removeAll(Collection<?> collectionToRemove) {
		boolean hasChanged = false;
		for(Object element : collectionToRemove)
		{
			hasChanged = this.remove(element) || hasChanged;  //must be in this order to avoid short circuit
		}
		return hasChanged;
	}

	@Override
	public boolean retainAll(Collection<?> collectionToRetain) {
		return dataList.retainAll(collectionToRetain);
	}

	@Override
	public void clear() {
		dataList.clear();
	}
	
	@Override
	public int hashCode() {
		return dataList.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if(obj == this) return true;
		if(!(obj instanceof Set<?>)) return false;

		Set<?> otherList = (Set<?>) obj;
		if(this.size() != otherList.size()) return false;
		return this.containsAll(otherList);
	}
	
	@Override
	public String toString() {
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append("{\"class\": \"");
		stringBuilder.append(this.getClass().getName());
		stringBuilder.append("\", \"hexHash\": \"");
		stringBuilder.append(Integer.toHexString(this.hashCode()));
		stringBuilder.append("\", \"data\": {");
		if(!this.isEmpty())
		{
			for(int index = 0; index < this.size(); index++)
			{
				stringBuilder.append('"');
				stringBuilder.append(dataList.get(index));
				stringBuilder.append("\": true");
				if(index+1 < this.size()) stringBuilder.append(", ");
			}
		}
		stringBuilder.append("}}");
		return stringBuilder.toString();
	}
	
	//note that it is not a live list. ie it is a copy
	public List<E> asList()
	{
		return new ArrayList<>(dataList);
	}

}
