package src.sameobject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

//TODO :add javadoc. note that it violates some of the interface because it does not use element.equals
public class SameObjectList<E> extends ArrayList<E> {
	private static final long serialVersionUID = 1L;

	//Uses super: size, isEmpty, iterator(all of them), toArray(both), add(all of them),
	//clear, get, set, constructors(sort of), hashCode, toString

	public SameObjectList(){}
	public SameObjectList(int initialCapacity){super(initialCapacity);}
	public SameObjectList(Collection<? extends E> initialElements){super(initialElements);}
	public SameObjectList(E[] initialElements){super(Arrays.asList(initialElements));}

	@Override
	public boolean contains(Object objectToFind) {
		for(int index=0; index < this.size(); index++)
		{
			if(this.get(index) == objectToFind) return true;
		}
		return false;
	}

	@Override
	public boolean containsAll(Collection<?> collectionToSearch) {
		for(Object objectToFind : collectionToSearch)
		{
			if(!this.contains(objectToFind)) return false;
		}
		return true;
	}

	@Override
	public boolean remove(Object objectToFind) {
		int index = this.indexOf(objectToFind);
		if(index == -1) return false;
		remove(index);
		return true;
	}

	@Override
	public E remove(int index) {
		return super.remove(index);
	}

	@Override
	public boolean removeAll(Collection<?> collectionToRemove) {
		boolean hasChanged = false;
		for(Object objectToRemove : collectionToRemove)
		{
			hasChanged = this.remove(objectToRemove) || hasChanged;  //must be in this order to avoid short circuit
		}
		return hasChanged;
	}

	@Override
	public boolean retainAll(Collection<?> collectionToRetain) {
		boolean hasChanged = false;
		SameObjectList<?> listToRetain = new SameObjectList<>(collectionToRetain);
		for(int index=0; index < this.size(); index++)
		{
			if(!listToRetain.contains(this.get(index)))
			{
				hasChanged = true;
				this.remove(index);
				index--;  //to prevent skipping an element
			}
		}
		return hasChanged;
	}

	@Override
	public int indexOf(Object objectToFind) {
		for(int index=0; index < this.size(); index++)
		{
			if(this.get(index) == objectToFind) return index;
		}
		return -1;
	}

	@Override
	public int lastIndexOf(Object objectToFind) {
		for(int index=this.size(); index > 0; index--)
		{
			if(this.get(index-1) == objectToFind) return index;
		}
		return -1;
	}

	@Override
	public List<E> subList(int fromIndex, int toIndex) {
		return new SameObjectList<>(super.subList(fromIndex, toIndex));
	}

	@Override
	public boolean equals(Object obj) {
		if(obj == this) return true;
		if(!(obj instanceof List<?>)) return false;

		List<?> otherList = (List<?>) obj;
		if(this.size() != otherList.size()) return false;
		return this.containsAll(otherList);
	}

	@Override
	public Object clone() {
		throw new RuntimeException(new CloneNotSupportedException());
	}
	
	@Override
	public String toString() {
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append("{\"class\": \"");
		stringBuilder.append(this.getClass().getName());
		stringBuilder.append("\", \"hexHash\": \"");
		stringBuilder.append(Integer.toHexString(this.hashCode()));
		stringBuilder.append("\", \"data\": ");
		stringBuilder.append(super.toString());
		stringBuilder.append('}');
		return stringBuilder.toString();
	}

}
