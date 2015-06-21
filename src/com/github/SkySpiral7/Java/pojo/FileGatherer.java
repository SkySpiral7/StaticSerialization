package com.github.SkySpiral7.Java.pojo;

import java.io.File;
import java.nio.file.Paths;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Deque;
import java.util.List;
import java.util.regex.Pattern;

public class FileGatherer {
	private File rootFolder;
	//TODO: use java.io.FileFilter instead of Pattern. Also see: javax.swing.filechooser.FileNameExtensionFilter
	private Pattern fileCriteria, subFolderCriteria, folderCriteria;
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
	/**
	 * This method returns all fields to their default values. The entire code is as follows:<code><pre>
rootFolder=new File(".");  //the default root is where this class's project (or executable jar) is located
folderCriteria=fileCriteria=subFolderCriteria=Pattern.compile("");  //matches everything (same as ".")
findFiles=true;
findFolders=false;
maxFinds=maxDepth=-1;</pre></code>
	 */
	public void resetAll()
	{
		rootFolder=new File(".");  //the default root is where this class's project (or executable jar) is located
		folderCriteria=fileCriteria=subFolderCriteria=Pattern.compile("");  //matches everything (same as ".")
		findFiles=true;
		findFolders=false;
		maxFinds=maxDepth=-1;
	}
   public FileGatherer(File rootFolder, Pattern fileCriteria) {
	   resetAll();
	   this.fileCriteria = fileCriteria;
		this.rootFolder = rootFolder;
	}
   /**
    * @param rootFolder the root which will be searched along with all subfolders
    * @param fileCriteria will compare using fileCriteria.matcher(thisFile.getName()).find() this includes extension type
    * @param folderCriteria will compare using folderCriteria.matcher(thisFile.getName()).find() to determine if folder matches the pattern
    * @param subFolderCriteria will compare using folderCriteria.matcher(thisFile.getName()).find() to determine if the folder should be explored
    * @param maxDepth pass -1 to have no maximum otherwise it only will go that number of folders down pass 0 for this folder only
    * @param maxFinds pass -1 to have no maximum
    * @param findFolders true if you want to check folder's names or false if you do not
    * @param findFiles whether or not you want non-directory (ie normal) files
    */
   //TODO: use builder pattern
   public FileGatherer(File rootFolder, Pattern fileCriteria, Pattern subFolderCriteria, Pattern folderCriteria, int maxDepth, int maxFinds, boolean findFolders, boolean findFiles)
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
   public static List<File> searchForFiles(File rootFolder, Pattern fileCriteria)
   {
	   return new FileGatherer(rootFolder, fileCriteria).performSearch();
   }
   public static List<File> searchForFiles(File rootFolder, Pattern fileCriteria, Pattern subFolderCriteria, Pattern folderCriteria, int maxDepth, int maxFinds, boolean findFolders, boolean findFiles)
   {
	   FileGatherer temp = new FileGatherer(rootFolder, fileCriteria, subFolderCriteria, folderCriteria, maxDepth, maxFinds, findFolders, findFiles);
	   return temp.performSearch();
   }
   public static List<File> searchForFolders(File rootFolder, Pattern folderCriteria)
   {
	   FileGatherer temp = new FileGatherer();
	   temp.setRootFolder(rootFolder);
	   temp.setFolderCriteria(folderCriteria);
	   temp.setFindFolders(true);
	   temp.setFindFiles(false);
	   return temp.performSearch();
   }
   public static List<File> searchForFolders(File rootFolder, Pattern fileCriteria, Pattern folderCriteria)
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
             if(thisFile.isDirectory() && subFolderCriteria.matcher(thisFile.getName()).find()) remaining.addAll(Arrays.asList(thisFile.listFiles()));

             if(!thisFile.isDirectory() && findFiles && fileCriteria.matcher(thisFile.getName()).find()) result.add(thisFile);
             else if(thisFile.isDirectory() && findFolders && folderCriteria.matcher(thisFile.getName()).find()) result.add(thisFile);

             if(maxFinds == result.size()) break;
       }

       Collections.sort(result);  //so that the folders will have some kind of order (in this case full path alphabetical ascending)
       return result;
 }
   //**************************************************************************
   //Generated getters and setters
   //**************************************************************************

   public Pattern getSubFolderCriteria() {
		return subFolderCriteria;
	}
	public void setSubFolderCriteria(Pattern subFolderCriteria) {
		this.subFolderCriteria = subFolderCriteria;
	}
	public Pattern getFolderCriteria() {
		return folderCriteria;
	}
	public void setFolderCriteria(Pattern folderCriteria) {
		this.folderCriteria = folderCriteria;
	}
	public Pattern getFileCriteria() {
		return fileCriteria;
	}
	public void setFileCriteria(Pattern fileCriteria) {
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
