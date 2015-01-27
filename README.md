Java
====
This repository is a holder for stand alone files or very small projects.
This was once part of the Miscellaneous repo but was separated to make it easier
to have a Java project in Eclipse. Note that the project is currently using JRE 1.8 although it can easily be changed to 1.6.

This repo largely contains code that has no tests and is possibly half finished.
The code might have unfinished or buggy functions but every other function worked under my use conditions.
Assuming I ran it at all that is. A lot of them only need Javadoc and are otherwise done.

#Tests
As expected "src" contains the code and "tests" contain the tests.
Testing an interface doesn't even make sense, likewise for beans (since they contain no logic).
The tests run off of junit-4.11 with hamcrest-all-1.3.

#Finished but Untested
1. SimpleLogger
2. IteratorExternal
3. ListIteratorExternal
4. DescendingListIterator
5. ReadOnlyIterator
6. ReadOnlyListIterator
7. InfinitelyLinkedList
8. MapEntryExternal
9. LinkedList
10. IdentityHashSet
11. SameObjectList, SameObjectMap, and SameObjectSet although they are deprecated

#Finished and Tested
1. Copyable (interface)
2. ModCountList (interface)
3. IntegerQuotient (bean)
