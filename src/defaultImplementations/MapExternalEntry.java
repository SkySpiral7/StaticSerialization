package src.defaultImplementations;

import java.util.Map;
import java.util.Objects;

import src.JsonHelper;

//created when entrySet is called. Can't be used as the map's internal data since it depends on the map
//also see: java.util.AbstractMap.SimpleEntry<K, V> and java.util.AbstractMap.SimpleImmutableEntry<K, V>
//TODO: make a sublist, iterator, list iterator, deque node. any more I can think of
public class MapExternalEntry<K,V> implements Map.Entry<K,V> {
	private Map<K,V> underlyingMap;
	private K key;

	public MapExternalEntry(Map<K,V> underlyingMap, K key)
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
        if(this==obj) return true;
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
			stringBuilder.append("}}");
	        return stringBuilder.toString();
		}
    }
}
