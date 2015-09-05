package com.github.SkySpiral7.Java.dataStructures;

import java.util.BitSet;
import java.util.EnumSet;
import java.util.HashSet;

/**
 * <p>A Bloom filter is a data structure in which objects can be inserted and queried to see if the object is contained.
 * A Bloom filter is storage efficient and the contains method is O(1) complexity.
 * Calling contains has false positives but no false negatives.</p>
 *
 * <p>A Bloom filter is the single easiest data structure to implement so I thought I would.
 * More specifically I wanted an antithesis to all the other Java Bloom filters I found online:
 * <a href="https://code.google.com/p/guava-libraries/source/browse/guava/src/com/google/common/hash/BloomFilter.java">guava</a>, 
 * <a href="https://github.com/MagnusS/Java-BloomFilter/blob/master/src/com/skjegstad/utils/BloomFilter.java#L160">MagnusS</a>,
 * <a href="https://github.com/Baqend/Orestes-Bloomfilter/blob/master/src/main/java/orestes/bloomfilter/BloomFilter.java">Baqend</a>, and
 * <a href="http://www.javamex.com/tutorials/collections/bloom_filter_java.shtml">javamex</a>.
 * Each of those implementations contain tons of useless code which adds overhead and confuses the API.</p>
 *
 * <p>Bloom filters are designed to be used by another data structure as an internal optimization.
 * They are only useful for large lists that are populated once and never changed afterward (it's impossible to remove elements from
 * a Bloom filter). Although the hash function(s) used are very important to the functionality of a Bloom filter
 * they are not tightly coupled with the filter.</p>
 *
 * <p>An example use case of a Bloom filter is for an airport's "no fly" list. Most names are not on this black list
 * and the list is long. A Bloom filter can instantly determine if a person's name is not on that list. However if the Bloom
 * filter returns true then the list must be searched directly comparing the names exactly to see if the name is actually
 * on the list.</p>
 *
 * @see HashSet HashSet for small sets (such as 100 elements).
 * @see EnumSet EnumSet for working with enums of the same class (no matter how big or small).
 */
public final class BloomFilter
{
	//initially hasher was public with a getter. But that's useless (and allows bad practices)
	private final BloomHash hasher;
	private final BitSet mask;

	/**
	 * @param hasher the hash function that will be used to convert objects to bits
	 * @param numberOfBits the size of the BitSet returned by hasher
	 */
	public BloomFilter(BloomHash hasher, int numberOfBits)
	{
		this.hasher = hasher;
		mask = new BitSet(numberOfBits);
	}

	/**
	 * Adds the element to this filter.
	 */
	public void add(Object element){mask.or(hasher.hash(element));}

	/**
	 * @return true if element might have been added or false if element definitely hasn't been
	 */
	public boolean contains(Object element)
	{
		final BitSet hashResult1 = hasher.hash(element);
		final BitSet hashResult2 = (BitSet) hashResult1.clone();
		//it's faster to clone then to call hash again
		hashResult1.and(mask);
		return hashResult1.equals(hashResult2);
		//return ((mask & hashResult) == hashResult);
	}

	/**
	 * @return the internal mask which is the bitwise or of all elements added. Note that if all
	 * bits are set then contains will always return true. Likewise if all bits are equally likely (which is absurd)
	 * then the percent chance of a false positive is: {@code setBits / totalBits *100}.
	 */
	public BitSet getMask(){return (BitSet) mask.clone();}

	/**
	 * This interface is a strategy pattern that the Bloom filter relies on to turn objects into bits.
	 * There is no default implementation because Bloom filters are always domain specific
	 * which makes a generic implementation impossible.
	 *
	 * @see #hash(Object)
	 */
	public static interface BloomHash
	{
		/**
		 * <p>This method is responsibly for hashing an object into a BitSet. The returned
		 * BitSet can't be 0 (all bits cleared)! If 0 is returned then calling add
		 * has no effect and contains will always return true.</p>
		 *
		 * <p>The returned BitSet should have as few set bits are possible (not 0!) with them as spread out as possible.
		 * Ideally every possible object would return exactly 1 bit which is uniquely mapped such that
		 * there are no collisions (this would prevent all false positives).
		 * This ideal is impossible (except for enums which is exactly what EnumSet does).</p>
		 *
		 * <p>The implementation should not simply return Object.hashCode because
		 * most implementations of hashCode use prime numbers for multiplies (to reduce collision).
		 * However this also causes many bits to be set which is toxic to a Bloom filter.
		 * Additionally Object.hashCode returns an int however 32 bits is too small of a range for an effective
		 * Bloom filter: the crowded range will cause many false positives.</p>
		 *
		 * @param element the object that was requested to be added to the Bloom filter.
		 * @return the BitSet that will be masked into the Bloom filter.
		 */
		public BitSet hash(Object element);
	}

}
