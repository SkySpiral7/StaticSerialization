Java
====
This repository is a holder for stand alone files or very small projects.
This was once part of the Miscellaneous repo but was separated to make it easier
to have a Java project in Eclipse. Note that the project is currently using JRE 1.8 although it can easily be changed to 1.6.

This repo largely contains code that has no tests and is possibly half finished.
The code might have unfinished or buggy functions but every other function worked under my use conditions.
Although that's assuming I ran the code at all. A lot of them only need Javadoc and are otherwise done.


#Tests
As expected "src" contains the code and "tests" contain the tests.
The tests run off of junit-4.11 with hamcrest-all-1.3.
It doesn't make sense to test an interface except for default and static methods.
I do not make tests for the sake of line coverage and to have a UT for each class.
Instead I only make meaningful unit tests: ones that test functionality that have a chance of being wrong.
The definition of a bean requires that they do not have functionality.

The unit tests are in the same package as the file being tested (but are in the tests folder instead of src).
The tests are named "UT_File.java" where "File" is the name of the class being tested.

The UT_FileToStringAdapter.java requires a few files to run.
.gitignore exists for the sole purpose of excluding largeFile.txt because it is larger than 4GB.
If you would like to test using the large file, the UT includes a method to recreate it.


#Works in progress
1. FileToStringAdapter
2. InfiniteInteger

#Finished but Untested
1. BasicSetTheory
2. BitWiseUtil
3. ComparableSugar
4. DequeNode
5. DequeNodeIterator
6. DescendingListIterator
7. FileGatherer
8. InfinitelyLinkedList
9. IteratorExternal
10. JsonHelper
11. JumpingIteratorExternalRandomAccess
12. LinkedList
13. ListIteratorExternal
14. MapEntryExternal
15. SimpleLogger

#Finished and Tested
1. JumpingIterator although an interface the static and default methods are tested

#Finished without anything to test
1. Copyable (interface)
2. Comparison (enum without functionality)
3. IdentityHashSet (delegates without functionality)
4. IntegerQuotient (bean)
5. JumpingIteratorDecoratorSequential (delegates without functionality)
6. ListIndexOutOfBoundsException (exception class)
7. ModCountList (interface)
8. ReadOnlyIterator (delegates without test-worthy functionality)
9. ReadOnlyListIterator (delegates without test-worthy functionality)
