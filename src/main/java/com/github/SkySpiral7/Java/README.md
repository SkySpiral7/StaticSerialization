File descriptions
=================
#BasicSetTheory
This utility class has some of the basic functionality of set theory.
Example methods include: union, intersection, difference, isProperSubset, cartesianProduct, and powerSet.


#BitWiseUtil
This utility class has some methods which internally use bitwise operations.
Example methods include: multiplyByPowerOf2, isPowerOf2, isEven, getLowestNBits, and getHighestNBits.


#ComparableSugar
This utility class has methods based on Comparable and Comparator.
These methods exist only for the sake of readability. The class using it is expected to have:

import static com.github.SkySpiral7.Java.pojo.Comparison.*;

import static com.github.SkySpiral7.Java.util.ComparableSugar.*;


#Comparison
This enum exists to be used by ComparableSugar. Although it can be equally meaningful to use them elsewhere.
They are: GREATER_THAN, LESS_THAN, EQUAL_TO, GREATER_THAN_OR_EQUAL_TO, LESS_THAN_OR_EQUAL_TO, NOT_EQUAL;


#Copyable
This interface defines only a single method which is used to make a copy of the object.
See the Javadoc for how this differs from clone.
The only method is: public T copy();


#DequeNode
This pojo is used as nodes for a deque data structure. The nodes are self-linking which is the only functionality defined.


#DequeNodeIterator
This class is a ListIterator for DequeNodes. It allows mutation (add, remove, set) and was harder to defined than I expected.
This can be returned by a linked list in order to implement the listIterator methods.
This file also has inner classes for ValueIterator and index agnostic versions of each.


#DescendingListIterator
A simple decorator class which makes implementing the List method descendingIterator easy to do.
This list iterator simple iterates backwards over a given list iterator.


#FileGatherer
A simple program to find files deeply and return a List<File>.

**Inputs**:

1. rootFolder: where to start.
2. fileCriteria and folderCriteria which is a Pattern that is compared to the file name to determine if it should be in the resulting list.
3. subFolderCriteria a Pattern that is compared to the folder name to determine if it should be explored or not
4. maxFinds and maxDepth use -1 for no limit. maxDepth is the number of folders down to go from root
5. findFolders and findFiles booleans to determine if files and/or folders should be included in resulting list. these trump fileCriteria and folderCriteria

**Outputs**: List<File> that meets the criteria


#FileToStringAdapter
This class is a wrapper around File. It extends File and has a method for many of the String methods. The String based methods
perform the action over the file contents (even files larger than Integer.MAX_VALUE). These methods include getting a substring
of the file contents and modifying the file contents.

**Inputs**: Has the same constructors as File and many methods of String.

**Outputs**: The file content mutators return nothing. For all others see File and String Javadocs.


#IdentityHashSet
A very simple set wrapper for IdentityHashMap. The set uses pointer equality instead of .equals to determine
if an object is redundant.


#InfiniteInteger
This class can represent any integer with perfect precision. This class has no maximum value but BigInteger does have a maximum value
which is approximately 2^(2^31). This class is convenient but is not designed to be efficient.


#InfinitelyLinkedList
This class is a list without a maximum size unlike an array which has a maximum of approximately 2^31 elements.
This data structure requires InfiniteInteger but should otherwise be efficient (as much as it can be which isn't much).


#IntegerQuotient
An immutable bean used to represent the results of integer division.


#IteratorExternal
A basic implementation of an iterator for a list. It was copied from AbstractList.Itr with few changes.
The list must be passed into this class's constructors.


#JsonHelper
This utility is used to quote strings as needed and return JSON assuming that the given object's toString returns JSON.


#JumpingIterator
An interface for a list iterator that can move more than one step at a time.
Each of the methods have defaults and a static version.
The methods are: jumpToBeginning, jumpByIndex, jumpToIndex, and jumpToEnd.


#JumpingIteratorDecoratorSequential
A decorator which converts a ListIterator into a JumpingIterator.


#JumpingIteratorExternalRandomAccess
A basic implementation of a random access JumpingIterator.
The list which has the data must be passed into this class's constructors.


#LinkedList
A simple linked list. I created it because I didn't like the JRE LinkedList and because the JRE version wasn't very child class friendly.


#ListIndexOutOfBoundsException
Much like the ArrayIndexOutOfBoundsException but for a list.


#ListIteratorExternal
A basic implementation of a list iterator for a list. It was copied from AbstractList.ListItr with few changes.
The list must be passed into this class's constructors.


#MapEntryExternal
A basic implementation of a map entry for a map. It was based on the external iterators.
The map must be passed into this class's constructors.


#ModCountList
This interface was created so that any class may look at the modCount which is defined in AbstractList.
The mod count is used by the external iterators to check for co-modification.


#ReadOnlyIterator
A decorator for an Iterator which doesn't allow mutations (the remove method).


#ReadOnlyListIterator
A decorator for a ListIterator which doesn't allow mutations (the methods: add, set, remove).


#SimpleLogger
This class is a very simple logger useful for quick and dirty debugging.
The only constructor takes a file. The methods are: getFile, append, appendLine, clear, and delete. They do what you'd expect.

**Inputs**: The file to be written to. Call methods to modify it.

**Outputs**: The file on disk will be modified.
