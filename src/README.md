File descriptions
=================
#FileGatherer.java
A simple program to find files deeply and return a List<File>.

**Inputs**:
1. rootFolder: where to start.
2. fileCriteria and folderCriteria which is a Pattern that is compared to the file name to determine if it should be in the resulting list.
3. subFolderCriteria a Pattern that is compared to the folder name to determine if it should be explored or not
4. maxFinds and maxDepth use -1 for no limit. maxDepth is the number of folders down to go from root
5. findFolders and findFiles booleans to determine if files and/or folders should be included in resulting list. these trump fileCriteria and folderCriteria

**Outputs**: List<File> that meets the criteria


#FileToStringAdapter.java
This class is a wrapper around File. It extends File and has a method for many of the String methods. The String based methods
perform the action over the file contents (even files larger than Integer.MAX_VALUE). These methods include getting a substring
of the file contents and modifying the file contents.

**Inputs**: Has the same constructors as File and many methods of String.

**Outputs**: The file content mutators return nothing. For all others see File and String docs.
