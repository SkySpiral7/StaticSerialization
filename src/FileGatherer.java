package src;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

public class FileGatherer {
	private File rootFolder;
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
   public List<File> performSearch()
   {
	   List<File> myFilePathsFound = new ArrayList<File>();
       List<File> unexploreredDirectories = new ArrayList<File>();
       if((!findFiles && !findFolders) || maxFinds==0) return myFilePathsFound;
       if(maxDepth!=-1) maxDepth+=countCharOccurrencesInString(rootFolder.getAbsolutePath(), '\\')+1;
  			//this math is done to convert maxDepth from relative depth to absolute depth
       		//if(maxDepth!=-1) is not necessary but is more clear
       unexploreredDirectories.add(rootFolder);
      while (!unexploreredDirectories.isEmpty())
      {
          File [] anUnorganizedArray = unexploreredDirectories.get(0).listFiles();
          File thisFile;
          int currentDepth;
         for (int anUnorganizedArrayIndex=0; anUnorganizedArrayIndex < anUnorganizedArray.length; anUnorganizedArrayIndex++)
         {
             thisFile=anUnorganizedArray[anUnorganizedArrayIndex];
             currentDepth=countCharOccurrencesInString(thisFile.getAbsolutePath(), '\\');
             if(thisFile.isDirectory() && currentDepth!=maxDepth && subFolderCriteria.matcher(thisFile.getName()).find()) unexploreredDirectories.add(thisFile);
             if(!thisFile.isDirectory() && findFiles && fileCriteria.matcher(thisFile.getName()).find()) myFilePathsFound.add(thisFile);
             else if(thisFile.isDirectory() && findFolders && folderCriteria.matcher(thisFile.getName()).find()) myFilePathsFound.add(thisFile);
             if(maxFinds==myFilePathsFound.size()) return myFilePathsFound;
         }
          unexploreredDirectories.remove(0);
      }
       Collections.sort(myFilePathsFound);  //so that the folders will have some kind of order (in this case full path alphabetical ascending)
       return myFilePathsFound;
   }
   //copied from MyTools. It is simple and I want this file to stand alone
   private static int countCharOccurrencesInString(String stringToSearch, char characterToFind)
   {
     int returnValue=0;
     for(int i=0; i < stringToSearch.length(); i++)
     {
        if(stringToSearch.charAt(i)==characterToFind) returnValue++;
     }
     return returnValue;
     //same as: return stringToSearch.split(Pattern.quote(""+characterToFind)).length-1;
     //same as: return stringToSearch.split("\\Q"+characterToFind+"\\E")).length-1;
     //this should be faster though since it doesn't need to create a substring with each match
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
