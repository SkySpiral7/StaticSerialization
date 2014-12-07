package src.sameobject;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

//TODO: add javadoc. note that it violates some of the interface because it does not use element.equals
//HashSet was right: using a map is the easiest way to implement a set. Can't be reverse since map needs to associate keys to values
public class SameObjectSet<E> implements Set<E> {

    /** Dummy value to associate with an Object in the backing Map.
     * The value is Boolean.TRUE. I didn't need to create a new object.
     * Plus it is truthy to make debugging slightly easier.*/
	private final static Boolean PRESENT = Boolean.TRUE; 
	private SameObjectMap<E, Boolean> dataMap;

	public SameObjectSet(){dataMap = new SameObjectMap<E, Boolean>();}
	public SameObjectSet(Collection<? extends E> initialElements){this(); this.addAll(initialElements);}
	public SameObjectSet(E[] initialElements){this(Arrays.asList(initialElements));}

	@Override
	public int size() {
		return dataMap.size();
	}

	@Override
	public boolean isEmpty() {
		return this.size() == 0;
	}

	@Override
	public boolean contains(Object objectToFind) {
		return dataMap.containsKey(objectToFind);
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
		return dataMap.keyList.iterator();
	}

	@Override
	public Object[] toArray() {
		return dataMap.keyList.toArray();
	}

	@Override
	public <T> T[] toArray(T[] destinationArray) {
		return dataMap.keyList.toArray(destinationArray);
	}

	@Override
	public boolean add(E newElement) {
		if(this.contains(newElement)) return false;
		return dataMap.put(newElement, PRESENT).booleanValue();  //haha nice
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
		if(!this.contains(objectToRemove)) return false;
		dataMap.remove(objectToRemove);
		return true;
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
		boolean hasChanged = false;
		hasChanged = dataMap.keyList.retainAll(collectionToRetain);
		dataMap.valueList = (SameObjectList<Boolean>) dataMap.valueList.subList(0, this.size());
		//only works because all values are the same
		return hasChanged;
	}

	@Override
	public void clear() {
		dataMap.clear();
	}
	
	@Override
	public int hashCode() {
		return dataMap.keyList.hashCode();
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
				stringBuilder.append(dataMap.keyList.get(index));
				stringBuilder.append("\": true");
				if(index+1 < this.size()) stringBuilder.append(", ");
			}
		}
		stringBuilder.append("}}");
		return stringBuilder.toString();
	}

}
