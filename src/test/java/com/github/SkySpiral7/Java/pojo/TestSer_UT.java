package com.github.SkySpiral7.Java.pojo;

import java.io.Externalizable;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import com.github.SkySpiral7.Java.Copyable;
import org.junit.Test;

import static com.github.SkySpiral7.Java.pojo.DeepSerializableMatcher.isDeeplySerializable;
import static org.junit.Assert.assertThat;

public class TestSer_UT
{

   @Test
   public void isSerializable()
   {
      DeepSerializableMatcher.ASSUMED_SERIALIZABLE_DEFAULTS.addAll(Arrays.asList(Comparator.class, Copyable.class, Comparable.class));
      assertThat(TestSer.class, isDeeplySerializable("empty"));
   }

}

class TestSer<T extends Serializable> implements Serializable
{
   private static final long serialVersionUID = 1L;

   public String f1;
   @SuppressWarnings("unused")
   private List<Map<Short, Comparable<Double>>> f2;
   public short f3;
   public InnerSer f4;
   public transient Object f5;
   public T f6;
   public static Object f7;

   public class Fun implements Serializable
   {
      private static final long serialVersionUID = 1L;
      public Byte f1;
   }
}

class OuterSer implements Copyable<OuterSer>, Serializable
{
   private static final long serialVersionUID = 1L;
   public Comparator<Double> f1;

   //Object f2;
   @Override
   public OuterSer copy()
   {
      return this;
   }
}

class InnerSer extends OuterSer implements Serializable
{
   private static final long serialVersionUID = 1L;
   public Double[][][] f1;
   @SuppressWarnings("unused")
   private float[][] f2;
   public Copyable<String>[][] f3;
   public Copyable<String>[][] f33;
   public TestSer<String>.Fun f4;
   public C1<String>[] f5;
   public List<? extends Serializable>[] f6;
   public Container.Contained f7;

   @Override
   public InnerSer copy()
   {
      return this;
   }
}

class C1<T> implements Serializable
{
   private static final long serialVersionUID = 1L;
   public T[] f1;
}

class Container
{
   public abstract static class Contained implements Externalizable
   {}
}
