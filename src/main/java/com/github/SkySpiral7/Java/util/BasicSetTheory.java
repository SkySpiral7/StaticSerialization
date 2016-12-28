package com.github.SkySpiral7.Java.util;

import java.util.*;

/**
 * <p>This is a utility class to perform some basic Set Theory operations.</p>
 *
 * <p>None of these methods mutate the parameters in any way.</p>
 *
 * <p>All functions except {@link #cartesianProduct} are only defined for sets
 * however since I must throw them into a set anyway for your convenience they
 * allow any Collection (except {@link #isSubset}, {@link #isProperSubset},
 * and {@link #uniqueCartesianProduct} which only make sense with sets).</p>
 */
public abstract class BasicSetTheory
{
   private BasicSetTheory() {}

   /**
    * <p>A union operation returns a set which contains all of the elements of both parameters.
    * This is also known as a full outer join.</p>
    *
    * <p>If left contains elements [A, B] and right contains [B, C] then the union
    * is [A, B, C].</p>
    *
    * <p>The order of the parameters does not matter but they must be parameterized to contain the same class.</p>
    *
    * <p>As a simple Ascii Venn diagram: (A {B) C} returns ABC</p>
    *
    * @return a set of all elements in both
    *
    * @throws IllegalArgumentException
    *       if either parameter is null
    */
   public static <E> Set<E> union(Collection<? extends E> left, Collection<? extends E> right)
   {
      if (left == null) throw new IllegalArgumentException("Parameter left can't be null.");
      if (right == null) throw new IllegalArgumentException("Parameter right can't be null.");
      Set<E> union = new HashSet<E>(left);
      union.addAll(new HashSet<E>(right));
      return union;
   }

   /**
    * <p>An intersection operation returns a set of common elements which both parameters contain.
    * This is also known as an inner join.</p>
    *
    * <p>If left contains elements [A, B] and right contains [B, C] then the intersection
    * is [B].</p>
    *
    * <p>The order of the parameters does not matter but they must be parameterized to contain the same class.</p>
    *
    * <p>As a simple Ascii Venn diagram: (A {B) C} returns B</p>
    *
    * @return a set of the elements in common
    *
    * @throws IllegalArgumentException
    *       if either parameter is null
    */
   public static <E> Set<E> intersection(Collection<? extends E> left, Collection<? extends E> right)
   {
      if (left == null) throw new IllegalArgumentException("Parameter left can't be null.");
      if (right == null) throw new IllegalArgumentException("Parameter right can't be null.");
      Set<E> intersection = new HashSet<>(left);
      intersection.retainAll(new HashSet<>(right));
      return intersection;
   }

   /**
    * <p>The symmetric difference of 2 sets is the set of elements that are not in common.
    * This is also known as an outer join. The Apache library calls this operation "sum"
    * which is misleading because it excludes some elements.</p>
    *
    * <p>If left contains elements [A, B] and right contains [B, C] then the intersection
    * is [A, C].</p>
    *
    * <p>Note that symmetric difference is the same as the combination of left difference and right difference.</p>
    *
    * <p>The order of the parameters does not matter but they must be parameterized to contain the same class.</p>
    *
    * <p>As a simple Ascii Venn diagram: (A {B) C} returns AC</p>
    *
    * @return a set of the elements that are different
    *
    * @throws IllegalArgumentException
    *       if either parameter is null
    */
   public static <E> Set<E> symmetricDifference(Collection<? extends E> left, Collection<? extends E> right)
   {
      if (left == null) throw new IllegalArgumentException("Parameter left can't be null.");
      if (right == null) throw new IllegalArgumentException("Parameter right can't be null.");
      Set<E> result = union(left, right);
      result.removeAll(intersection(left, right));
      return result;
   }

   /**
    * <p>Left difference of 2 sets is the set of elements that are in left that are not in right.
    * This is also known subtract and left outer join.</p>
    *
    * <p>If left contains elements [A, B] and right contains [B, C] then the left difference
    * is [A]. You can also say left minus right or right subtracted from left.</p>
    *
    * <p>The order of the parameters is important and they must be parameterized to contain the same class.
    * Note that leftDifference(left, right) is the same as rightDifference(right, left).</p>
    *
    * <p>As a simple Ascii Venn diagram: (A {B) C} returns A</p>
    *
    * @param left
    *       start with this set
    * @param right
    *       and remove these elements
    * @return a set of the elements that only exist in left
    *
    * @throws IllegalArgumentException
    *       if either parameter is null
    */
   public static <E> Set<E> leftDifference(Collection<? extends E> left, Collection<? extends E> right)
   {
      if (left == null) throw new IllegalArgumentException("Parameter left can't be null.");
      if (right == null) throw new IllegalArgumentException("Parameter right can't be null.");
      Set<E> result = new HashSet<>(left);
      result.removeAll(right);
      return result;
   }

   /**
    * <p>Right difference of 2 sets is the set of elements that are in right that are not in left.
    * This is also known subtract and right outer join.</p>
    *
    * <p>If left contains elements [A, B] and right contains [B, C] then the right difference
    * is [C]. You can also say right minus left or left subtracted from right.</p>
    *
    * <p>The order of the parameters is important and they must be parameterized to contain the same class.
    * Note that rightDifference(left, right) is the same as leftDifference(right, left).</p>
    *
    * <p>As a simple Ascii Venn diagram: (A {B) C} returns C</p>
    *
    * @param left
    *       remove these elements from right
    * @param right
    *       start with this set
    * @return a set of the elements only exist in right
    *
    * @throws IllegalArgumentException
    *       if either parameter is null
    * @see #leftDifference(Collection, Collection)
    */
   public static <E> Set<E> rightDifference(Collection<? extends E> left, Collection<? extends E> right)
   {
      if (left == null) throw new IllegalArgumentException("Parameter left can't be null.");
      if (right == null) throw new IllegalArgumentException("Parameter right can't be null.");
      //these if checks are needed so that the error message is not backwards
      return leftDifference(right, left);
   }

   /**
    * <p>child is a subset of parent if and only if all elements of child exist in parent.
    * The parent can be equal to child or have more elements. Note that the empty set is a
    * subset of all sets.</p>
    *
    * <p>If child contains elements [B, C] and parent contains [A, B, C] then child is a subset of parent.</p>
    * <ul>
    * <li><code>isSubset([B,C], [A,B,C]) == true</code></li>
    * <li><code>isSubset([A,B,C], [A,B,C]) == true</code></li>
    * <li><code>isSubset([], [A,B,C]) == true</code></li>
    * </ul>
    *
    * <p>The order of the parameters is important and they must be parameterized to contain the same class.
    * When using this class think of its name being in between the parameters such as:
    * (child isSubset parent). Or: is child a subset of parent?</p>
    *
    * @return true if child is a subset of parent
    *
    * @throws IllegalArgumentException
    *       if either parameter is null
    * @see #isProperSubset(Set, Set)
    */
   public static <E> boolean isSubset(Set<? extends E> child, Set<? extends E> parent)
   {
      if (child == null) throw new IllegalArgumentException("Parameter child can't be null.");
      if (parent == null) throw new IllegalArgumentException("Parameter parent can't be null.");
      //these if checks are needed so that the error message is not confusing
      return leftDifference(child, parent).isEmpty();
   }

   /**
    * <p>child is a proper subset of parent if and only if all elements of child exist in parent
    * and parent is not equal to child. The parent must have more elements than child. Note that the empty set is a
    * proper subset of all sets except itself.</p>
    *
    * <p>If child contains elements [B, C] and parent contains [A, B, C] then child is a proper subset of parent.</p>
    * <ul>
    * <li><code>isSubset([B,C], [A,B,C]) == true</code></li>
    * <li><code>isSubset([A,B,C], [A,B,C]) == false</code></li>
    * <li><code>isSubset([], [A,B,C]) == true</code></li>
    * <li><code>isSubset([], []) == false</code></li>
    * </ul>
    *
    * <p>The order of the parameters is important and they must be parameterized to contain the same class.
    * When using this class think of its name being in between the parameters such as:
    * (child isProperSubset parent). Or: is child a proper subset of parent?</p>
    *
    * @return true if child is a proper subset of parent
    *
    * @throws IllegalArgumentException
    *       if either parameter is null
    * @see #isSubset(Set, Set)
    */
   public static <E> boolean isProperSubset(Set<? extends E> child, Set<? extends E> parent)
   {
      return (isSubset(child, parent) && !child.equals(parent));
   }

   //I can't use vargs because of heap pollution
   //I can't return E[][] because I can't create the needed array
   private static <E> List<List<E>> cartesianProduct(E[][] inputArray)
   {
      if (inputArray == null) throw new IllegalArgumentException("Parameter inputArray can't be null.");
      if (inputArray.length < 2) throw new IllegalArgumentException("Parameter inputArray's first dimension must have at least 2 elements.");
      List<List<E>> cartesianProduct = new ArrayList<>();

      for (E[] collection : inputArray)
      {
         if (collection.length == 0) return new ArrayList<>();
         //TODO: figure out how to get this to work. probably need a bitwise combination like powerSet
         //javadoc it after finished
      }
      return cartesianProduct;
   }

   /**
    * <p>The cartesianProduct of 2 sets is a set of all combinations of the input elements.
    * This is also known as the product set or simply product. This is also known as the cross product or simply cross.</p>
    *
    * <p>Set theory only works with sets so normally cartesian product would return a set. I instead
    * returned lists so that you may have every possible combination with duplicates. For unique combinations
    * see uniqueCartesianProduct.</p>
    *
    * <p>If A is ["Left", "Right"] and B is ["Hand", "Foot"] then AxB is
    * [["Left", "Hand"], ["Left", "Foot"], ["Right", "Hand"], ["Right", "Foot"]].</p>
    *
    * <p>["Left Hand", "Left Foot", "Right Hand", "Right Foot"] x ["Red", "Green", "Yellow", "Blue"] ==
    * [["Left Hand", "Red"], ["Left Hand", "Green"], ["Left Hand", "Yellow"], ["Left Hand", "Blue"], ["Left Foot", "Red"], ["Left Foot", "Green"], ["Left Foot",
    * "Yellow"], ["Left Foot", "Blue"], ["Right Hand", "Red"], ["Right Hand", "Green"], ["Right Hand", "Yellow"], ["Right Hand", "Blue"], ["Right Foot", "Red"],
    * ["Right Foot", "Green"], ["Right Foot", "Yellow"], ["Right Foot", "Blue"]]
    * </p>
    *
    * <p>If A is [1,2,3] then AxA is [[1, 1], [1, 2], [1, 3], [2, 1], [2, 2], [2, 3], [3, 1], [3, 2], [3, 3]].</p>
    *
    * <p>The order of the parameters is important and they must be parameterized to contain the same class.
    * The element from collection A will be the first element of the combination and B will be the second.
    * For example [1,2]x[3,4] == [[1,3], [1,4], [2,3], [2,4]]
    * but [3,4]x[1,2] == [[3,1], [4,1], [3,2], [4,2]].</p>
    *
    * <p>The size of the cartesian product (the returned list) will be equal to collectionA.size() * collectionB.size().
    * If either parameter is empty then an empty list will be returned.
    * Each element of the returned list will have a size equal to the number of parameters, in this case always 2,
    * ie cartesianProduct(A, B).get(x).size() == 2 for any x (within range).</p>
    *
    * <p>Calling cartesianProduct multiple times will likely not result in the desired outcome. For example:
    * [1,2]x[-1,-2]x[3,4] == [[[1, -1], 3], [[1, -1], 4], [[1, -2], 3], [[1, -2], 4], [[2, -1], 3], [[2, -1], 4], [[2, -2], 3], [[2, -2], 4]].</p>
    *
    * @return every possible combination of the collections
    *
    * @throws IllegalArgumentException
    *       if either parameter is null
    * @see #uniqueCartesianProduct(Set, Set)
    */
   public static <E> List<List<E>> cartesianProduct(Collection<? extends E> collectionA, Collection<? extends E> collectionB)
   {
      if (collectionA == null) throw new IllegalArgumentException("Parameter collectionA can't be null.");
      if (collectionB == null) throw new IllegalArgumentException("Parameter collectionB can't be null.");
      List<List<E>> cartesianProduct = new ArrayList<>();
      if (collectionA.isEmpty() || collectionB.isEmpty()) return cartesianProduct;  //fast path for B. A is here for symmetry

      for (E elementFromA : collectionA)
      {
         for (E elementFromB : collectionB)
         {
            List<E> combination = new ArrayList<>();
            combination.add(elementFromA);
            combination.add(elementFromB);
            cartesianProduct.add(combination);
         }
      }
      return cartesianProduct;
   }

   //might have to use this for cartesianProduct(E[][])
   //[1,2]x[-1,-2]x[3,4] == [[[1, -1], 3], [[1, -1], 4], [[1, -2], 3], [[1, -2], 4], [[2, -1], 3], [[2, -1], 4], [[2, -2], 3], [[2, -2], 4]]
   //desire: [[1, -1, 3], [1, -1, 4], [1, -2, 3], [1, -2, 4], [2, -1, 3], [2, -1, 4], [2, -2, 3], [2, -2, 4]]
   private static <E> List<List<E>> anotherCartesianProduct(List<List<E>> previousCartesianProduct, Collection<? extends E> anotherCollection)
   {
      if (previousCartesianProduct == null) throw new IllegalArgumentException("Parameter previousCartesianProduct can't be null.");
      if (anotherCollection == null) throw new IllegalArgumentException("Parameter anotherCollection can't be null.");
      List<List<E>> cartesianProduct = new ArrayList<>();
      if (previousCartesianProduct.isEmpty() || anotherCollection.isEmpty())
         return cartesianProduct;  //fast path for anotherCollection. previousCartesianProduct is here for symmetry

      for (List<E> previousElement : previousCartesianProduct)
      {
         for (E anotherElement : anotherCollection)
         {
            List<E> combination = new ArrayList<>(previousElement);
            combination.add(anotherElement);
            cartesianProduct.add(combination);
         }
      }
      return cartesianProduct;
   }

   /**
    * <p>This function returns the cartesian product as defined by set theory since the parameters are sets and it returns sets.
    * This behaves in the same way as the function cartesianProduct but returns a set of sets instead.</p>
    *
    * <p>For example if A is [1,2,3] then AxA is [[1, 2], [1, 3], [2, 3]] because
    * [1,1] etc is [1] due to uniqueness which is not a valid combination
    * and [2,1] etc is the same as [1,2] because sets do not have order.</p>
    *
    * <p>If isSubset(child, parent) then uniqueCartesianProduct(child, parent)
    * == uniqueCartesianProduct(parent, parent). If intersection(A, B).isEmpty()
    * then uniqueCartesianProduct(A, B) == cartesianProduct(A, B) except that Set&lt;Set&lt;E&gt;&gt; != List&lt;List&lt;E&gt;&gt;.</p>
    *
    * <p>The order of the parameters does not matter but they must be parameterized to contain the same class.
    * For example [1,2]x[3,4] == [4,3]x[1,2] == [[1, 3], [4, 1], [2, 3], [2, 4]].</p>
    *
    * <p>If either parameter is empty then an empty set will be returned.
    * Each element of the returned set will have a size equal to the number of parameters, in this case always 2,
    * ie uniqueCartesianProduct(A, B).get(x).size() == 2 for any x (within range).</p>
    *
    * <p>Calling uniqueCartesianProduct multiple times will likely not result in the desired outcome. For example:
    * [1,2]x[-1,-2]x[3,4] == [[[1, -1], 3], [[1, -1], 4], [[1, -2], 3], [[1, -2], 4], [[2, -1], 3], [[2, -1], 4], [[2, -2], 3], [[2, -2], 4]].</p>
    *
    * @return every possible combination of the collections without repeated elements
    *
    * @throws IllegalArgumentException
    *       if either parameter is null
    * @see #cartesianProduct(Collection, Collection)
    */
   public static <E> Set<Set<E>> uniqueCartesianProduct(Set<? extends E> collectionA, Set<? extends E> collectionB)
   {
      if (collectionA == null) throw new IllegalArgumentException("Parameter collectionA can't be null.");
      if (collectionB == null) throw new IllegalArgumentException("Parameter collectionB can't be null.");
      Set<Set<E>> uniqueCartesianProduct = new HashSet<>();
      if (collectionA.isEmpty() || collectionB.isEmpty()) return uniqueCartesianProduct;  //fast path for B. A is here for symmetry
      //if(collectionA.equals(collectionB)) has no fast path

      for (E elementFromA : collectionA)
      {
         for (E elementFromB : collectionB)
         {
            Set<E> combination = new HashSet<>();
            combination.add(elementFromA);
            combination.add(elementFromB);
            if (combination.size() == 2) uniqueCartesianProduct.add(combination);
            //size of 1 means elementFromA.equals(elementFromB) which is not allowed as a combination
         }
      }
      return uniqueCartesianProduct;
      //I notice that uniqueCartesianProduct could call powerSet then remove all elements that don't have a size of 2 but that would be slower
   }

   /**
    * <p>The powerSet is a set of all possible subsets of the input.
    * Note that they are not proper subsets which means the powerSet will always return at least
    * a set containing the input set.</p>
    *
    * <p><code>powerSet([]) == [[]]. powerSet([1]) == [[], [1]]. powerSet([1, 2]) == [[], [1], [2], [1,2]].</code></p>
    *
    * <p><code>powerSet([1, 2, 3]) == [[], [1], [2], [3], [1,2], [1,3], [2,3], [1,2,3]].</code></p>
    *
    * <p><code>powerSet([1, 2, 3, 4]) == [[], [1], [2], [3], [4], [1,2], [1,3], [1,4], [2,3], [2,4], [3,4], [1,2,3], [1,2,4], [1,3,4], [2,3,4],
    * [1,2,3,4]].</code></p>
    *
    * <p>The size of the power set (the returned set) will be equal to 2^originalSet.size().</p>
    *
    * @return every possible subset of the originalSet
    *
    * @throws IllegalArgumentException
    *       if either parameter is null
    * @see #uniqueCartesianProduct(Set, Set)
    * @see #isSubset(Set, Set)
    */
   //the code is based on: http://stackoverflow.com/questions/1670862/obtaining-a-powerset-of-a-set-in-java/14818944#14818944
   public static <T> Set<Set<T>> powerSet(Set<? extends T> originalSet)
   {
      if (originalSet == null) throw new IllegalArgumentException("Parameter originalSet can't be null.");
      if (originalSet.size() >= 31) throw new ArithmeticException("Overflow. A Collection can't contain 2^31 items or more.");
      //Integer.MAX_VALUE == 2,147,483,647 == (2^31)-1

      List<T> originalList = new ArrayList<>(originalSet);
      //the order doesn't matter. it is put into a list so that each element will have an index
      byte originalSize = (byte) originalList.size();
      Set<Set<T>> powerSet = new HashSet<>();

      //(1 << originalSize) will be at most 1 << 30 == 2^30 meaning int will not overflow
      for (int combination = 0; combination < (1 << originalSize); combination++)
      {
         Set<T> subset = new HashSet<>();  //could end up being empty or equal to the originalSet
         for (byte elementIndex = 0; elementIndex < originalSize; elementIndex++)
         {
            //branch prediction will not be happy with this
            if (((combination >> elementIndex) & 1) == 1) subset.add(originalList.get(elementIndex));
            //each bit of combination represents true or false if that element should be included in the subset
            //so shifting over the elementIndex then bitwise and 1 will capture just that bit
         }
         powerSet.add(subset);
      }

      return powerSet;
   }

}
