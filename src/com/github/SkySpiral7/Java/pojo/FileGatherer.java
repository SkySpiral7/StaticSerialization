package com.github.SkySpiral7.Java.pojo;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.nio.file.AccessDeniedException;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Pattern;
import javax.swing.filechooser.FileNameExtensionFilter;

public class FileGatherer
{
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
   /*
    * This method returns all fields to their default values. The entire code is as follows:<code><pre>
rootFolder = new File(".");  //the default root is where this class's project (or executable jar) is located
folderCriteria = fileCriteria = subFolderCriteria = Filters.ACCEPT_ALL;
findFiles = true;
findFolders = false;
maxFinds = maxDepth = -1;</pre></code>
	 */
   public FileGatherer()
   {
      rootFolder = new File(".");  //the default root is where this class's project (or executable jar) is located
      folderCriteria = fileCriteria = subFolderCriteria = Filters.ACCEPT_ALL;
      findFiles = true;
      findFolders = false;
      maxFinds = maxDepth = -1;
   }

   public FileGatherer(File rootFolder)
   {
      this(rootFolder, Filters.ACCEPT_ALL);
   }

   public FileGatherer(File rootFolder, FileFilter fileCriteria)
   {
      this();
      this.fileCriteria = fileCriteria;
      this.rootFolder = rootFolder;
   }

   /**
    * @param rootFolder
    *       the root which will be searched along with all subfolders
    * @param fileCriteria
    *       if accept returns true then the file will be added to the search results
    * @param folderCriteria
    *       if accept returns true then the folder will be added to the search results
    * @param subFolderCriteria
    *       if accept returns false then the folder will not be searched. Note that this is called on rootFolder
    * @param maxDepth
    *       pass -1 to have no maximum otherwise it only will go that number of folders down pass 0 for this folder only
    * @param maxFinds
    *       pass -1 to have no maximum
    * @param findFolders
    *       true if you want to check folder's names or false if you do not
    * @param findFiles
    *       whether or not you want non-directory (ie normal) files
    */
   public FileGatherer(Builder builder)
   {
      //this();  //doesn't need since it defines everything
      this.subFolderCriteria = builder.subFolderCriteria;
      this.folderCriteria = builder.folderCriteria;
      this.fileCriteria = builder.fileCriteria;
      this.maxDepth = builder.maxDepth;
      this.maxFinds = builder.maxFinds;
      this.findFolders = builder.findFolders;
      this.findFiles = builder.findFiles;
      this.rootFolder = builder.rootFolder;
   }

   /**
    * Also see Files.walk(Path) which (if passed no file visitors) does the same thing (but is more efficient).
    *
    * @see java.nio.file.Files#walk(Path, FileVisitOption...)
    */
   public static List<File> searchForFiles(File rootFolder)
   {
      return searchForFiles(rootFolder, Filters.ACCEPT_ALL);
   }

   public static List<File> searchForFiles(File rootFolder, FileFilter fileCriteria)
   {
      return new FileGatherer(rootFolder, fileCriteria).search();
   }

   public static List<File> searchForFolders(File rootFolder, FileFilter folderCriteria)
   {
      FileGatherer temp = new FileGatherer();
      temp.rootFolder = rootFolder;
      temp.folderCriteria = folderCriteria;
      temp.findFolders = true;
      temp.findFiles = false;
      return temp.search();
   }

   public static List<File> searchForFilesWithExtension(File rootFolder, String... extensions)
   {
      return new FileGatherer(rootFolder, Filters.acceptExtensions(extensions)).search();
   }

   public List<File> search()
   {
      List<File> result = new ArrayList<File>();
      Deque<File> remaining = new ArrayDeque<File>();

      //TODO: use more nio (have builder convert)
      if (maxDepth != -1) maxDepth += Paths.get(rootFolder.getAbsolutePath()).getNameCount() + 1;
      //this math is done to convert maxDepth from relative depth to absolute depth
      //+1 make it rootFolder's children not rootFolder itself

      remaining.add(rootFolder);
      while (!remaining.isEmpty())
      {
         final File thisFile = remaining.pollLast();
         if (maxDepth != -1 && maxDepth < Paths.get(thisFile.getAbsolutePath()).getNameCount()) continue;
         try
         {
            if (thisFile.isDirectory()) Files.newDirectoryStream(thisFile.toPath());
         }
         catch (final AccessDeniedException e)
         {
            continue;  //ignore the directory
         }
         catch (final IOException e)
         {
            throw new RuntimeException(e);
         }
         if (thisFile.isDirectory() && subFolderCriteria.accept(thisFile)) remaining.addAll(Arrays.asList(thisFile.listFiles()));

         if (!thisFile.isDirectory() && findFiles && fileCriteria.accept(thisFile)) result.add(thisFile);
         else if (thisFile.isDirectory() && findFolders && folderCriteria.accept(thisFile)) result.add(thisFile);

         if (maxFinds == result.size()) break;
      }

      Collections.sort(result);  //so that the results will have some kind of order (in this case full path alphabetical ascending)
      return result;
   }

   public static class Filters
   {
      public static final FileFilter ACCEPT_ALL = (File file) -> true;
      public static final FileFilter FILTER_HIDDEN = (File file) -> (!file.isHidden() && !file.getName().startsWith("."));

      public static FileFilter acceptExtensions(String... extensions)
      {
         //the name isn't required and it can't be seen
         final FileNameExtensionFilter extensionFilter = new FileNameExtensionFilter(null, extensions);
         //notice how extensionFilter is only created once and used for each call to accept
         return (File file) -> (!file.isDirectory() && extensionFilter.accept(file));
      }

      public static FileFilter acceptNamePattern(Pattern pattern)
      {
         return (File file) -> pattern.matcher(file.getName()).find();
      }
   }

   public static class Builder
   {
      private File rootFolder;
      private FileFilter fileCriteria, subFolderCriteria, folderCriteria;
      private int maxDepth, maxFinds;
      private boolean findFolders, findFiles;

      public Builder()
      {
         rootFolder = new File(".");  //the default root is where this class's project (or executable jar) is located
         folderCriteria = fileCriteria = subFolderCriteria = Filters.ACCEPT_ALL;
         findFiles = true;
         findFolders = false;
         maxFinds = maxDepth = -1;
      }

      public FileGatherer build()
      {
         if (!findFiles && !findFolders) throw new IllegalStateException("Nothing to find. findFiles=findFolders=false");
         if (maxFinds == 0) throw new IllegalStateException("Nothing to find. maxFinds=0");
         return new FileGatherer(this);
      }

      public Builder unlimitedDepth()
      {
         this.maxDepth = -1;
         return this;
      }

      public Builder maxDepth(int maxDepth)
      {
         if (maxDepth != -1 && maxDepth < 1) throw new IllegalArgumentException("Expected -1 or >= 1. Actual: " + maxDepth);
         this.maxDepth = maxDepth;
         return this;
      }

      public Builder unlimitedFinds()
      {
         this.maxFinds = -1;
         return this;
      }

      public Builder maxFinds(int maxFinds)
      {
         if (maxFinds != -1 && maxFinds < 1) throw new IllegalArgumentException("Expected -1 or >= 1. Actual: " + maxFinds);
         this.maxFinds = maxFinds;
         return this;
      }

      public Builder rootFolder(File rootFolder)
      {
         Objects.requireNonNull(rootFolder);
         if (!rootFolder.exists()) throw new IllegalArgumentException(rootFolder + " doesn't exist");
         if (!rootFolder.isDirectory()) throw new IllegalArgumentException(rootFolder + " isn't a directory");
         this.rootFolder = rootFolder;
         return this;
      }

      //**************************************************************************
      //Basic field setting methods for a builder
      //**************************************************************************
      public Builder subFolderCriteria(FileFilter subFolderCriteria)
      {
         Objects.requireNonNull(rootFolder);
         this.subFolderCriteria = subFolderCriteria;
         return this;
      }

      public Builder folderCriteria(FileFilter folderCriteria)
      {
         Objects.requireNonNull(rootFolder);
         this.folderCriteria = folderCriteria;
         return this;
      }

      public Builder fileCriteria(FileFilter fileCriteria)
      {
         Objects.requireNonNull(rootFolder);
         this.fileCriteria = fileCriteria;
         return this;
      }

      public Builder findFolders(boolean findFolders)
      {
         this.findFolders = findFolders;
         return this;
      }

      public Builder findFiles(boolean findFiles)
      {
         this.findFiles = findFiles;
         return this;
      }

      //**************************************************************************
      //Rest of file is generated getters
      //**************************************************************************
      public FileFilter getSubFolderCriteria()
      {
         return subFolderCriteria;
      }

      public FileFilter getFolderCriteria()
      {
         return folderCriteria;
      }

      public FileFilter getFileCriteria()
      {
         return fileCriteria;
      }

      public int getMaxDepth()
      {
         return maxDepth;
      }

      public int getMaxFinds()
      {
         return maxFinds;
      }

      public boolean isFindFolders()
      {
         return findFolders;
      }

      public boolean isFindFiles()
      {
         return findFiles;
      }

      public File getRootFolder()
      {
         return rootFolder;
      }
   }

   //**************************************************************************
   //Rest of file is generated getters
   //**************************************************************************
   public FileFilter getSubFolderCriteria()
   {
      return subFolderCriteria;
   }

   public FileFilter getFolderCriteria()
   {
      return folderCriteria;
   }

   public FileFilter getFileCriteria()
   {
      return fileCriteria;
   }

   public int getMaxDepth()
   {
      return maxDepth;
   }

   public int getMaxFinds()
   {
      return maxFinds;
   }

   public boolean isFindFolders()
   {
      return findFolders;
   }

   public boolean isFindFiles()
   {
      return findFiles;
   }

   public File getRootFolder()
   {
      return rootFolder;
   }
}
