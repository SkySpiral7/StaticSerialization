package src;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

//TODO: add Javadoc
public abstract class BasicSetTheory {
   private BasicSetTheory(){}

   //(A {B) C} returns ABC
   public static <E> Set<E> union(Collection<? extends E> left, Collection<? extends E> right)
   {
       //aka full outer join
       Set<E> union = new HashSet<E>(left);
       union.addAll(new HashSet<E>(right));
       return union;
   }

   //(A {B) C} returns B
   public static <E> Set<E> intersect(Collection<? extends E> left, Collection<? extends E> right)
   {
       //aka inner join
       Set<E> intersection = new HashSet<E>(left);
       intersection.retainAll(new HashSet<E>(right));
       return intersection;
   }

   //(A {B) C} returns AC
   public static <E> Set<E> symmetricDifference(Collection<? extends E> left, Collection<? extends E> right)
   {
       //aka outer. apache calls this sum
       Set<E> result = union(left, right);
       result.removeAll(intersect(left, right));
       return result;
   }

   //(A {B) C} returns A
   public static <E> Set<E> leftDifference(Collection<? extends E> left, Collection<? extends E> right)
   {
       //aka subtract
       Set<E> result = new HashSet<E>(left);
       result.removeAll(right);
       return result;
   }

   //(A {B) C} returns C
   public static <E> Set<E> rightDifference(Collection<? extends E> left, Collection<? extends E> right)
   {
       Set<E> result = new HashSet<E>(right);
       result.removeAll(left);
       return result;
   }

   public static <E> boolean isSubSet(Set<? extends E> left, Set<? extends E> right)
   {
       return leftDifference(left, right).isEmpty();
   }

   public static <E> boolean isProperSubSet(Set<? extends E> left, Set<? extends E> right)
   {
       return (isSubSet(left, right) && !left.equals(right));
   }

   //({1, 2} and {red, white} is {(1, red), (1, white), (2, red), (2, white)}
   //{1, 2, 3} and {1, 2, 3} is { {1,1}, {1,2}, {1,3}, {2,1}, {2,2}, {2,3}, {3,1}, {3,2}, {3,3} }
   //if either is empty then empty is returned
   public static <E> List<List<E>> cartesianProduct(Collection<? extends E> left, Collection<? extends E> right)
   {
       List<List<E>> result = new ArrayList<>();
      for (E leftElement : left)
      {
         for (E rightElement : right)
         {
             List<E> row = new ArrayList<E>();
             row.add(leftElement);
             row.add(rightElement);
             result.add(row);
         }
      }
       return result;
   }

   public static <E> Set<Set<E>> uniqueCartesianProduct(Set<? extends E> left, Set<? extends E> right)
   {
       Set<Set<E>> result = new HashSet<>();
      for (E leftElement : left)
      {
         for (E rightElement : right)
         {
             Set<E> row = new HashSet<E>();
             row.add(leftElement);
             row.add(rightElement);
             if(row.size() != 1) result.add(row);
         }
      }
       return result;
   }

   //{} is { {} }
   //{1} is { {}, {1} }
   //{1, 2} is { {}, {1}, {2}, {1,2} }
   //{1, 2, 3} is { {}, {1}, {2}, {3}, {1,2}, {1,3}, {2,3}, {1,2,3} }
   //{1, 2, 3, 4} is { {}, {1}, {2}, {3}, {4}, {1,2}, {1,3}, {1,4}, {2,3}, {2,4}, {3,4}, {1,2,3}, {1,2,4}, {1,3,4}, {2,3,4}, {1,2,3,4} }

   //{1, 2, 3} x {1, 2, 3} is { {1,1}, {1,2}, {1,3}, {2,1}, {2,2}, {2,3}, {3,1}, {3,2}, {3,3} }
   //but without order: { {1,1}, {1,2}, {1,3}, {2,2}, {2,3}, {3,3} }
   //and ignore paired with self: { {1,2}, {1,3}, {2,3} } this is it

   //{1, 2, 3, 4} x self unique is: {1,2}, {1,3}, {1,4}, {2,3}, {2,4}, {3,4}
   public static <E> Set<Set<E>> powerSet(Set<? extends E> left)
   {
       Set<Set<E>> result = new HashSet<>();
       result.add(new HashSet<E>(left));  //always contains itself

      for (E leftElement : left)
      {
          Set<E> row = new HashSet<E>(left);
          row.remove(leftElement);
          Set<Set<E>> subResult = powerSet(row);
         for (Set<E> subSet : subResult)
         {
             result.add(subSet);
         }
      }

       return result;
   }
}
