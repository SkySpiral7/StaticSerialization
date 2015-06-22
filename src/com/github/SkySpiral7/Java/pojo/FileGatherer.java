package com.github.SkySpiral7.Java.pojo;

import java.io.File;
import java.io.FileFilter;
import java.nio.file.Paths;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Deque;
import java.util.List;

import javax.swing.filechooser.FileNameExtensionFilter;

public class FileGatherer {
	private File rootFolder;
	private FileFilter fileCriteria, subFolderCriteria, folderCriteria;
	private int maxDepth, maxFinds;
	private boolean findFolders, findFiles;
	   /*
	    * @param rootFolder the root which will be searched along with all subfolders
	    * @param fileCriteria will compare using fileCriteria.matcher(thisFile.getName()).find() this includes extension type
	    * @param folderCriteria will compare using folderCriteria.matcher(thisFile.getName()).find() to determine if folder matches the pattern
	    * @param subFolderCriteria will compare using folderCriteria.matcher(thisFile.getName()).find() to determine if the folder should be explored
	    * @param maxDepth pass -1 to have no maximum otherwise it only will go that number of folders down pass 0 for this folder only
	    * @param maxFinds pass -1 to have no maximum
	    * @param findFolders true if you want to check folder's names or false if you do not
	    * @param findFiles whether or not you want non-directory (ie normal) files
	    * @return a list of files with matching names
	    */
	public FileGatherer(){resetAll();}
	public static final FileFilter ACCEPT_ALL = (File file)->{return true;};
	public static final FileFilter FILTER_HIDDEN = (File file)->{return (!file.isHidden() && !file.getName().startsWith("."));};
	public static FileFilter acceptExtensions(String... extensions) {
		//the name isn't required and it can't be seen
		final FileNameExtensionFilter extensionFilter = new FileNameExtensionFilter(null, extensions);
		//notice how extensionFilter is only created once and used for each call to accept
		return (File file)->{return (!file.isDirectory() && extensionFilter.accept(file));};
	}
	/**
	 * This method returns all fields to their default values. The entire code is as follows:<code><pre>
rootFolder = new File(".");  //the default root is where this class's project (or executable jar) is located
folderCriteria = fileCriteria = subFolderCriteria = ACCEPT_ALL;
findFiles = true;
findFolders = false;
maxFinds = maxDepth = -1;</pre></code>
	 */
	public void resetAll()
	{
		rootFolder = new File(".");  //the default root is where this class's project (or executable jar) is located
		folderCriteria = fileCriteria = subFolderCriteria = ACCEPT_ALL;
		findFiles = true;
		findFolders = false;
		maxFinds = maxDepth = -1;
	}
   public FileGatherer(File rootFolder, FileFilter fileCriteria) {
	   resetAll();
	   this.fileCriteria = fileCriteria;
		this.rootFolder = rootFolder;
	}
   /**
    * @param rootFolder the root which will be searched along with all subfolders
    * @param fileCriteria if accept returns true then the file will be added to the search results
    * @param folderCriteria if accept returns true then the folder will be added to the search results
    * @param subFolderCriteria if accept returns false then the folder will not be searched. Note that this is called on rootFolder
    * @param maxDepth pass -1 to have no maximum otherwise it only will go that number of folders down pass 0 for this folder only
    * @param maxFinds pass -1 to have no maximum
    * @param findFolders true if you want to check folder's names or false if you do not
    * @param findFiles whether or not you want non-directory (ie normal) files
    */
   //TODO: use builder pattern
   public FileGatherer(File rootFolder, FileFilter fileCriteria, FileFilter subFolderCriteria, FileFilter folderCriteria, int maxDepth, int maxFinds, boolean findFolders, boolean findFiles)
   {
	   //resetAll();  //doesn't need since it defines everything
		this.subFolderCriteria = subFolderCriteria;
		this.folderCriteria = folderCriteria;
		this.fileCriteria = fileCriteria;
		this.maxDepth = maxDepth;
		this.maxFinds = maxFinds;
		this.findFolders = findFolders;
		this.findFiles = findFiles;
		this.rootFolder = rootFolder;
	}
   public static List<File> searchForFiles(File rootFolder, FileFilter fileCriteria)
   {
	   return new FileGatherer(rootFolder, fileCriteria).performSearch();
   }
   public static List<File> searchForFiles(File rootFolder, FileFilter fileCriteria, FileFilter subFolderCriteria, FileFilter folderCriteria, int maxDepth, int maxFinds, boolean findFolders, boolean findFiles)
   {
	   FileGatherer temp = new FileGatherer(rootFolder, fileCriteria, subFolderCriteria, folderCriteria, maxDepth, maxFinds, findFolders, findFiles);
	   return temp.performSearch();
   }
   public static List<File> searchForFolders(File rootFolder, FileFilter folderCriteria)
   {
	   FileGatherer temp = new FileGatherer();
	   temp.setRootFolder(rootFolder);
	   temp.setFolderCriteria(folderCriteria);
	   temp.setFindFolders(true);
	   temp.setFindFiles(false);
	   return temp.performSearch();
   }
   public static List<File> searchForFolders(File rootFolder, FileFilter fileCriteria, FileFilter folderCriteria)
   {
	   FileGatherer temp = new FileGatherer(rootFolder, fileCriteria);
	   temp.setFolderCriteria(folderCriteria);
	   temp.setFindFolders(true);
	   //temp.setFindFiles(true);  //default
	   return temp.performSearch();
   }
   public List<File> search(){return performSearch();};  //alias
   public List<File> performSearch() {
       List<File> result = new ArrayList<File>();
       Deque<File> remaining = new ArrayDeque<File>();

       if((!findFiles && !findFolders) || maxFinds == 0) return result;
       //TODO: use more nio (after builder)
       if(maxDepth != -1) maxDepth += Paths.get(rootFolder.getAbsolutePath()).getNameCount() +1;
  			//this math is done to convert maxDepth from relative depth to absolute depth
       		//+1 make it rootFolder's children not rootFolder itself

       remaining.add(rootFolder);
       while (!remaining.isEmpty())
       {
             File thisFile = remaining.pollLast();
             if(maxDepth != -1 && maxDepth < Paths.get(thisFile.getAbsolutePath()).getNameCount()) continue;
             if(thisFile.isDirectory() && subFolderCriteria.accept(thisFile)) remaining.addAll(Arrays.asList(thisFile.listFiles()));

             if(!thisFile.isDirectory() && findFiles && fileCriteria.accept(thisFile)) result.add(thisFile);
             else if(thisFile.isDirectory() && findFolders && folderCriteria.accept(thisFile)) result.add(thisFile);

             if(maxFinds == result.size()) break;
       }

       Collections.sort(result);  //so that the folders will have some kind of order (in this case full path alphabetical ascending)
       return result;
 }
   //**************************************************************************
   //Generated getters and setters
   //**************************************************************************

   public FileFilter getSubFolderCriteria() {
		return subFolderCriteria;
	}
	public void setSubFolderCriteria(FileFilter subFolderCriteria) {
		this.subFolderCriteria = subFolderCriteria;
	}
	public FileFilter getFolderCriteria() {
		return folderCriteria;
	}
	public void setFolderCriteria(FileFilter folderCriteria) {
		this.folderCriteria = folderCriteria;
	}
	public FileFilter getFileCriteria() {
		return fileCriteria;
	}
	public void setFileCriteria(FileFilter fileCriteria) {
		this.fileCriteria = fileCriteria;
	}
	public int getMaxDepth() {
		return maxDepth;
	}
	public void setMaxDepth(int maxDepth) {
		this.maxDepth = maxDepth;
	}
	public int getMaxFinds() {
		return maxFinds;
	}
	public void setMaxFinds(int maxFinds) {
		this.maxFinds = maxFinds;
	}
	public boolean isFindFolders() {
		return findFolders;
	}
	public void setFindFolders(boolean findFolders) {
		this.findFolders = findFolders;
	}
	public boolean isFindFiles() {
		return findFiles;
	}
	public void setFindFiles(boolean findFiles) {
		this.findFiles = findFiles;
	}
	public File getRootFolder() {
		return rootFolder;
	}
	public void setRootFolder(File rootFolder) {
		this.rootFolder = rootFolder;
	}
}
