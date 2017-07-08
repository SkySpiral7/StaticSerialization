package com.github.SkySpiral7.Java.util;

import java.util.regex.Pattern;

import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class StringUtil_UT
{
   @Test
   public void countCharOccurrences() throws Exception
   {
      assertThat(StringUtil.countCharOccurrences("aaa", 'a'), is(3));
      assertThat(StringUtil.countCharOccurrences("bbb", 'B'), is(0));
   }

   @Test
   public void regexFoundInString() throws Exception
   {
      assertThat(StringUtil.regexFoundInString("caab", "a+b"), is(true));
   }

   @Test
   public void regexReplaceFirst() throws Exception
   {
      assertThat(StringUtil.regexReplaceFirst("caAba", Pattern.compile("a+", Pattern.CASE_INSENSITIVE), "F"), is("cFba"));
   }

   @Test
   public void regexReplaceAll() throws Exception
   {
      assertThat(StringUtil.regexReplaceAll("caAba", Pattern.compile("a+", Pattern.CASE_INSENSITIVE), "F"), is("cFbF"));
   }

   @Test
   public void literalReplaceFirst() throws Exception
   {
      assertThat(StringUtil.literalReplaceFirst("a+b+", "+", "-"), is("a-b+"));
   }

   @Test
   public void literalReplaceAll() throws Exception
   {
      assertThat(StringUtil.literalReplaceAll("a+b+", "+", "-"), is("a-b-"));
   }

   @Test
   public void regexSplit() throws Exception
   {
      assertThat(StringUtil.regexSplit("1A2b3", Pattern.compile("[ab]", Pattern.CASE_INSENSITIVE)), is(new String[]{"1", "2", "3"}));
   }

   @Test
   public void literalSplit() throws Exception
   {
      assertThat(StringUtil.literalSplit("a+ b+ c", "+ "), is(new String[]{"a", "b", "c"}));
   }

}
