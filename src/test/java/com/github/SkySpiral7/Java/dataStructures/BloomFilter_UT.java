package com.github.SkySpiral7.Java.dataStructures;

import java.util.BitSet;

import com.github.SkySpiral7.Java.dataStructures.BloomFilter.BloomHash;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public final class BloomFilter_UT
{

   @Test
   public void bloomFilter()
   {
      final BloomFilter bloomFilter = new BloomFilter(new BloomHash()
      {
         @Override
         public BitSet hash(Object element){return BitSet.valueOf(new long[]{element.hashCode()});}
         //although bad practice, the hashCode is returned for the sake of easy testing
      }, 32);

      assertEquals(BitSet.valueOf(new long[]{0}), bloomFilter.getMask());
      assertFalse(bloomFilter.contains(1));

      bloomFilter.add(1);
      bloomFilter.add(2);
      bloomFilter.add(4);

      assertEquals(BitSet.valueOf(new long[]{7}), bloomFilter.getMask());
      assertTrue(bloomFilter.contains(2));
      assertTrue(bloomFilter.contains(3));  //false positive is expected behavior
      assertFalse(bloomFilter.contains(64));  //mask contains no matching bits
      assertFalse(bloomFilter.contains(21));  //mask contains some matching bits
      //this unit test is complete: absolutely all requirements have been tested
   }

}
