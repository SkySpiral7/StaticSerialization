package src.sameobject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import javax.naming.OperationNotSupportedException;

//TODO :add javadoc. note that it violates some of the interface because it does not use element.equals
//key and value can both be null. also point out the plentiful constructors
public final class SameObjectMap<K,V> implements Map<K,V>
{
	private SameObjectList<K> keyList;
	private SameObjectList<V> valueList;

	public SameObjectMap(){this.clear();}
	public SameObjectMap(List<K> initialKeyList, List<V> initialValueList)
	{
		this.keyList = new SameObjectList<>(initialKeyList);
		this.valueList = new SameObjectList<>(initialValueList);
	}
	public SameObjectMap(K[] initialKeyArray, V[] initialValueArray){this(Arrays.asList(initialKeyArray), Arrays.asList(initialValueArray));}
	public SameObjectMap(List<K> initialKeyList, V[] initialValueArray){this(initialKeyList, Arrays.asList(initialValueArray));}
	public SameObjectMap(K[] initialKeyArray, List<V> initialValueList){this(Arrays.asList(initialKeyArray), initialValueList);}
	public SameObjectMap(Map<? extends K, ? extends V> otherMap){this(); this.putAll(otherMap);}

	@Override
	public int size() {
		return keyList.size();
	}

	@Override
	public boolean isEmpty() {
        return size() == 0;
	}

	@Override
	public boolean containsKey(Object key) {
		return keyList.contains(key);
	}

	@Override
	public boolean containsValue(Object value) {
		return valueList.contains(value);
	}

	@Override
	public V get(Object key) {
		int index = keyList.indexOf(key);
		if(index == -1) throw new NoSuchElementException(key+" was not found in map. Please use containsKey if that is your desired behavior.");
		return valueList.get(index);
	}

	@Override
	public V put(K key, V value) {
		int index = keyList.indexOf(key);
		if(index != -1) return valueList.set(index, value);
		keyList.add(key);
		valueList.add(value);
		return value;
	}

	@Override
	public void putAll(Map<? extends K, ? extends V> otherMap) {
		if(this == otherMap) return;
		if(this.getClass().equals(otherMap.getClass()))
		{
			@SuppressWarnings("unchecked")
			//this will throw if children of K and V are involved
			SameObjectMap<K, V> otherSameObjectMap = (SameObjectMap<K, V>) otherMap;
			for(int index = 0; index < otherSameObjectMap.keyList.size(); index++)
			{
				this.put(otherSameObjectMap.keyList.get(index), otherSameObjectMap.valueList.get(index));
			}
		}
		Set<? extends K> otherKeySet = otherMap.keySet();
		//I can't use entrySet because of some generics issue
		for(K otherKey : otherKeySet)
		{
			this.put(otherKey, otherMap.get(otherKey));
		}
	}

	@Override
	public V remove(Object key) {
		int index = keyList.indexOf(key);
		if(index == -1) throw new NoSuchElementException(key+" was not found in map. Please use containsKey if that is your desired behavior.");
		return valueList.remove(index);
	}

	@Override
	public void clear() {
		keyList = new SameObjectList<>();
		valueList = new SameObjectList<>();
	}

	@Override
	public Set<K> keySet() {
		throw new RuntimeException("It is not possible to return a keySet because sets uses .equals for uniqueness", new OperationNotSupportedException());
	}

	@Override
	public Collection<V> values() {
		return new ArrayList<>(valueList);  //return a copy in a list that acts normal
	}

	@Override
	public Set<java.util.Map.Entry<K, V>> entrySet() {
		throw new RuntimeException("It is not possible to return an entrySet because sets uses .equals for uniqueness", new OperationNotSupportedException());
	}
	
	@Override
	public String toString() {
		StringBuilder str = new StringBuilder();
		str.append(super.toString());
		str.append('{');
		if(this.isEmpty())
		{
			str.append('}');
			return str.toString();
		}
		str.append('{');
		str.append(keyList.get(0));
		str.append(", ");
		str.append(valueList.get(0));
		str.append('}');
		for(int index = 1; index < keyList.size(); index++)
		{
			str.append(", ");
			str.append('{');
			str.append(keyList.get(index));
			str.append(", ");
			str.append(valueList.get(index));
			str.append('}');
		}
		str.append('}');
		return str.toString();
	}
	
	@Override
	public boolean equals(Object obj) {
		if(this == obj) return true;
		if(obj == null) return false;
		if(!this.getClass().equals(obj.getClass())) return false;

		@SuppressWarnings("unchecked")
		//this will throw if children of K and V are involved
		SameObjectMap<K, V> other = (SameObjectMap<K, V>) obj;

		//if(size() != other.size()) return false;  //covered by keyList.equals
		if(!this.keyList.equals(other.keyList)) return false;
		if(!this.valueList.equals(other.valueList)) return false;
		return true;
	}
	
	@Override
	public int hashCode() {
		int hash = 1;
		hash = hash * 17 + keyList.hashCode();
		hash = hash * 31 + valueList.hashCode();
		return hash;
	}

}
