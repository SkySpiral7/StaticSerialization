package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import org.junit.jupiter.api.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

class BooleanArraySerializableStrategy_UT
{
   @Test
   public void compress_matches_whenRoundTrip()
   {
      final boolean[] flagArray = {
         false, true, false, true,
         false, false, true, true,

         false, true, true, true,
         true, true, true, true
      };
      final byte[] bytes = BooleanArraySerializableStrategy.compress(flagArray);
      //it's 0xcafe but printed in binary is readable (backwards)
      assertThat(bytes, is(new byte[]{(byte) 0b1100_1010, (byte) 0b1111_1110}));
      assertThat(BooleanArraySerializableStrategy.decompress(bytes, flagArray.length), is(flagArray));
   }

   @Test
   public void compress_handlesSmall_whenRoundTrip()
   {
      final boolean[] flagArray = {
         false, true
      };
      final byte[] bytes = BooleanArraySerializableStrategy.compress(flagArray);
      //printed in binary is readable (backwards)
      assertThat(bytes, is(new byte[]{(byte) 0b0000_0010}));
      assertThat(BooleanArraySerializableStrategy.decompress(bytes, flagArray.length), is(flagArray));
   }
}
