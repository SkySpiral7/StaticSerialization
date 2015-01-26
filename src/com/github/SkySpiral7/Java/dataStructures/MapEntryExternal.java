package com.github.SkySpiral7.Java.dataStructures;

import java.util.AbstractMap;
import java.util.Map;
import java.util.Objects;

import com.github.SkySpiral7.Java.util.JsonHelper;

//TODO: make a sublist, iterator, list iterator, deque node. any more I can think of
/**
 * This Map.Entry can be created when Map.entrySet is called but can't be used to represent a map's
 * internal data this class depends on a map already having data. This class is thread safe and modifications
 * will affect the underlying map. Therefore setValue will throw if the underlying map throws on modification.
 *
 * @param <K> the data type of the key of the Map.Entry
 * @param <V> the data type of the value of the Map.Entry
 * 
 * @see AbstractMap.SimpleEntry
 * @see AbstractMap.SimpleImmutableEntry
 */
//TODO: javadoc everything in all files
public class MapEntryExternal<K,V> implements Map.Entry<K,V> {
	private final Map<K,V> underlyingMap;
	private final K key;

	public MapEntryExternal(Map<K,V> underlyingMap, K key)
	{
		this.underlyingMap = underlyingMap;
		this.key = key;
	}

	@Override public K getKey(){return key;}

	@Override
	public V getValue() {
		synchronized (underlyingMap) {
			confirmExistence();
			return underlyingMap.get(key);
		}
	}

	@Override
	public V setValue(V value) {
		synchronized (underlyingMap) {
			confirmExistence();
			return underlyingMap.put(key, value);
		}
	}

	private void confirmExistence() {
		if(!underlyingMap.containsKey(key)) throw new IllegalStateException("This entry no longer exists in the map.");
	}

	@Override
	public boolean equals(Object obj) {
        if(this == obj) return true;
        if(!(obj instanceof Map.Entry)) return false;
        Map.Entry<?,?> otherEntry = (Map.Entry<?,?>)obj;
        if(!Objects.equals(this.key, otherEntry.getKey())) return false;
        //this.getValue() calls this.confirmExistence(). otherEntry might not be doing this and is responsible for its value
        return Objects.equals(this.getValue(), otherEntry.getValue());
	}

	//as per super. same for equals
	@Override
	public int hashCode() {
		synchronized (underlyingMap) {
			int keyHash=Objects.hashCode(key);
			int valueHash=Objects.hashCode(this.getValue());  //this.getValue() calls this.confirmExistence()
	        return keyHash^valueHash;
		}
	}

	@Override
	public String toString() {
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append(JsonHelper.toStringHeader(this));
		stringBuilder.append('{');
		synchronized (underlyingMap) {
			stringBuilder.append(JsonHelper.stringify(key));
			stringBuilder.append(": ");
			stringBuilder.append(JsonHelper.stringify(this.getValue()));  //this.getValue() calls this.confirmExistence()
		}
		stringBuilder.append("}}");
        return stringBuilder.toString();
    }
}
