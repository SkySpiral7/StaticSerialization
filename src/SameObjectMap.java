package src;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

import javax.naming.OperationNotSupportedException;

public final class SameObjectMap<K,V> implements Map<K,V>
{

	private ArrayList<K> keyList;
	private ArrayList<V> valueList;
	
	public SameObjectMap()
	{
		this.clear();
	}
	
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
		//I can't use return keyList.contains(key); because ArrayList.contains uses .equals
		for(K keyInMap : keyList)
		{
			if(keyInMap == key) return true;
		}
		return false;
	}

	@Override
	public boolean containsValue(Object value) {
		//I can't use return valueList.contains(value); because ArrayList.contains uses .equals
		for(V valueInMap : valueList)
		{
			if(valueInMap == value) return true;
		}
		return false;
	}

	@Override
	public V get(Object key) {
		//I can't use ArrayList.indexOf because it uses .equals
		for(int index = 0; index < keyList.size(); index++)
		{
			if(keyList.get(index) == key) return valueList.get(index);
		}
		throw new IllegalArgumentException(key+" was not found in map. Please use containsKey if that is your desired behavior.");
	}

	@Override
	public V put(K key, V value) {
		//I can't use ArrayList.indexOf because it uses .equals
		for(int index = 0; index < keyList.size(); index++)
		{
			if(keyList.get(index) == key){valueList.set(index, value); return value;}
		}
		keyList.add(key);
		valueList.add(value);
		//ArrayList is not a set so add works as expected
		return value;
	}

	@Override
	public V remove(Object key) {
		//I can't use ArrayList.indexOf because it uses .equals
		for(int index = 0; index < keyList.size(); index++)
		{
			if(keyList.get(index) == key)
			{
				keyList.remove(index);
				return valueList.remove(index);
			}
		}
		throw new IllegalArgumentException(key+" was not found in map. Please use containsKey if that is your desired behavior.");
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
	public void clear() {
		keyList = new ArrayList<>();
		valueList = new ArrayList<>();
	}

	@Override
	public Set<K> keySet() {
		throw new RuntimeException("It is not possible to return a keySet because sets uses .equals for uniqueness", new OperationNotSupportedException());
	}

	@Override
	public Collection<V> values() {
		return new ArrayList<>(valueList);  //return a copy
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
		
		if(size() != other.size()) return false;

		//I can't use ArrayList.contains, indexOf, or equals because they all use element.equals
		for(int index = 0; index < keyList.size(); index++)
		{
			if(keyList.get(index) != other.keyList.get(index)) return false;
			if(valueList.get(index) != other.valueList.get(index)) return false;
		}
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
