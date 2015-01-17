package src.sameobject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Set;

import src.JsonHelper;
import src.defaultImplementations.MapEntryExternal;

//key and value can both be null. also point out the plentiful constructors
/**
 * @deprecated use java.util.IdentityHashMap instead
 * @see IdentityHashMap
 */
public class SameObjectMap<K,V> implements Map<K,V>
{
	private final SameObjectList<K> keyList;
	private final SameObjectList<V> valueList;

	/**
	 * The no-arg construct creates an empty map.
	 */
	public SameObjectMap()
	{
		keyList = new SameObjectList<>();
		valueList = new SameObjectList<>();
	}
	public SameObjectMap(List<K> initialKeyList, List<V> initialValueList)
	{
		Objects.requireNonNull(initialKeyList);
		Objects.requireNonNull(initialValueList);
		this.keyList = new SameObjectList<>(initialKeyList);
		this.valueList = new SameObjectList<>(initialValueList);
	}
	public SameObjectMap(K[] initialKeyArray, V[] initialValueArray){this(Arrays.asList(initialKeyArray), Arrays.asList(initialValueArray));}
	public SameObjectMap(List<K> initialKeyList, V[] initialValueArray){this(initialKeyList, Arrays.asList(initialValueArray));}
	public SameObjectMap(K[] initialKeyArray, List<V> initialValueList){this(Arrays.asList(initialKeyArray), initialValueList);}
	public SameObjectMap(Map<? extends K, ? extends V> otherMap){this(); Objects.requireNonNull(otherMap); this.putAll(otherMap);}

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
		if(index == SameObjectList.ELEMENT_NOT_FOUND) throw new NoSuchElementException(key+" was not found in map. Please use containsKey if that is your desired behavior.");
		return valueList.get(index);
	}

	@Override
	public V put(K key, V value) {
		int index = keyList.indexOf(key);
		if(index != SameObjectList.ELEMENT_NOT_FOUND) return valueList.set(index, value);
		keyList.add(key);
		valueList.add(value);
		return value;
	}

	@Override
	public void putAll(Map<? extends K, ? extends V> otherMap) {
		if(this == otherMap) return;
		Objects.requireNonNull(otherMap);
		if(this.getClass().equals(otherMap.getClass()))
		{
			//TODO: not sure if this works
			SameObjectMap<? extends K, ? extends V> otherSameObjectMap = (SameObjectMap<? extends K, ? extends V>) otherMap;
			for(int index = 0; index < otherSameObjectMap.keyList.size(); index++)
			{
				this.put(otherSameObjectMap.keyList.get(index), otherSameObjectMap.valueList.get(index));
			}
		}
		Set<? extends K> otherKeySet = otherMap.keySet();
		//I can't use entrySet because of some kind of generics issue
		//well I could cast it like I do above but I'd rather not have 2 points of failure
		for(K otherKey : otherKeySet)
		{
			this.put(otherKey, otherMap.get(otherKey));
		}
	}

	@Override
	public V remove(Object key) {
		int index = keyList.indexOf(key);
		if(index == SameObjectList.ELEMENT_NOT_FOUND) throw new NoSuchElementException(key+" was not found in map. Please use containsKey if that is your desired behavior.");
		return valueList.remove(index);
	}

	@Override
	public void clear() {
		keyList.clear();
		valueList.clear();
	}

	@Override
	public Set<K> keySet() {
		return new SameObjectSet<>(keyList);
	}

	@Override
	public Collection<V> values() {
		return new ArrayList<>(valueList);  //return a copy in a list that acts normal
	}

	@Override
	public Set<Map.Entry<K, V>> entrySet() {
		SameObjectSet<Map.Entry<K, V>> entrySet = new SameObjectSet<>();
		for(int index = 0; index < this.size(); index++)
		{
			entrySet.add(new MapEntryExternal<K,V>(this, keyList.get(index)));
		}
		return entrySet;
	}
	
	@Override
	public String toString() {
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append(JsonHelper.toStringHeader(this));
		stringBuilder.append('{');
		for(int index = 0; index < keyList.size(); index++)
		{
			stringBuilder.append(JsonHelper.stringify(keyList.get(index)));
			stringBuilder.append(": ");
			stringBuilder.append(JsonHelper.stringify(valueList.get(index)));
			if(index+1 < keyList.size()) stringBuilder.append(", ");
		}
		stringBuilder.append("}}");
		return stringBuilder.toString();
	}
	
	@Override
	public boolean equals(Object obj) {
		if(this == obj) return true;
		if(!(obj instanceof Map<?,?>)) return false;

		Map<?, ?> other = (Map<?, ?>) obj;
		if(size() != other.size()) return false;

		for(Object key : other.keySet())
		{
			if(!this.containsKey(key)) return false;
			if(this.get(key) != other.get(key)) return false;
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
