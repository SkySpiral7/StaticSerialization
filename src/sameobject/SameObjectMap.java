package src.sameobject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

//TODO :add javadoc. note that it violates some of the interface because it does not use element.equals
//key and value can both be null. also point out the plentiful constructors
public final class SameObjectMap<K,V> implements Map<K,V>
{
	private SameObjectList<K> keyList;
	private SameObjectList<V> valueList;

	public SameObjectMap()
	{
		keyList = new SameObjectList<>();
		valueList = new SameObjectList<>();
	}
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
			//this will throw if children of K or V are involved
			SameObjectMap<K, V> otherSameObjectMap = (SameObjectMap<K, V>) otherMap;
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
		if(index == -1) throw new NoSuchElementException(key+" was not found in map. Please use containsKey if that is your desired behavior.");
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
			entrySet.add(new Entry<K,V>(keyList.get(index), valueList.get(index), keyList, valueList));
		}
		return entrySet;
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
			for(int index = 0; index < keyList.size(); index++)
			{
				stringBuilder.append('"');
				stringBuilder.append(keyList.get(index));
				stringBuilder.append("\": \"");
				stringBuilder.append(valueList.get(index));
				stringBuilder.append('"');
				if(index+1 < keyList.size()) stringBuilder.append(", ");
			}
		}
		stringBuilder.append("}}");
		return stringBuilder.toString();
	}
	
	@Override
	public boolean equals(Object obj) {
		if(this == obj) return true;
		if(obj == null) return false;
		if(!this.getClass().equals(obj.getClass())) return false;

		SameObjectMap<?, ?> other = (SameObjectMap<?, ?>) obj;

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
	
    private static final class Entry<K,V> implements Map.Entry<K,V> {
    	private K key;
    	private V value;
    	private SameObjectList<K> keyList;
    	private SameObjectList<V> valueList;
    	
    	public Entry(K key, V value, SameObjectList<K> keyList, SameObjectList<V> valueList)
    	{
    		this.key = key;
    		this.value = value;
    		this.keyList = keyList;
    		this.valueList = valueList;
    	}
    	
		@Override public K getKey(){return key;}
		@Override public V getValue(){return value;}

		@Override
		public V setValue(V value) {
			int index = keyList.indexOf(key);
			if(index != -1) throw new IllegalStateException("This entry no longer exists in the map.");
			return valueList.set(index, value);
		}
    }

}
