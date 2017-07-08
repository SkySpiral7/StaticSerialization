package com.github.SkySpiral7.Java.util;

import java.util.Comparator;
import java.util.Objects;

import com.github.SkySpiral7.Java.pojo.Comparison;

import static com.github.SkySpiral7.Java.pojo.Comparison.EQUAL_TO;
import static com.github.SkySpiral7.Java.pojo.Comparison.GREATER_THAN;
import static com.github.SkySpiral7.Java.pojo.Comparison.GREATER_THAN_OR_EQUAL_TO;
import static com.github.SkySpiral7.Java.pojo.Comparison.LESS_THAN;
import static com.github.SkySpiral7.Java.pojo.Comparison.LESS_THAN_OR_EQUAL_TO;
import static com.github.SkySpiral7.Java.pojo.Comparison.NOT_EQUAL;

public class ComparableSugar
{
   private ComparableSugar(){}

   public static final byte THIS_LESSER = -1;  //aka me first
   public static final byte THIS_EQUAL = 0;
   public static final byte THIS_GREATER = 1;  //aka you first

   public static <T> boolean is(Comparable<T> left, Comparison operation, T right)
   {
      Objects.requireNonNull(right);
      return isComparisonResult(left.compareTo(right), operation);
   }

   public static <T> boolean is(T left, Comparison operation, T right, Comparator<T> accordingTo)
   {
      Objects.requireNonNull(left);
      Objects.requireNonNull(right);
      return isComparisonResult(accordingTo.compare(left, right), operation);
   }

   public static boolean isComparisonResult(int comparisonResult, Comparison operation)
   {
      Objects.requireNonNull(operation);
      //switching on result is cleaner (more readable) and smaller (less comparisons and code) than switching on operation
      switch (Integer.signum(comparisonResult))
      {
         case 0:
            return (operation == EQUAL_TO || operation == GREATER_THAN_OR_EQUAL_TO || operation == LESS_THAN_OR_EQUAL_TO);
         case 1:
            return (operation == NOT_EQUAL || operation == GREATER_THAN_OR_EQUAL_TO || operation == GREATER_THAN);
         //case -1:
         default:
            return (operation == NOT_EQUAL || operation == LESS_THAN_OR_EQUAL_TO || operation == LESS_THAN);
      }
   }

   public static <T extends Comparable<T>> T min(T left, T right)
   {
      if (is(left, LESS_THAN_OR_EQUAL_TO, right)) return left;
      return right;
   }

   public static <T extends Comparable<T>> T max(T left, T right)
   {
      if (is(left, GREATER_THAN_OR_EQUAL_TO, right)) return left;
      return right;
   }

}
