#include <windows.h>

#include <stdlib.h>
#include <sys/stat.h>
#include <io.h>
#include <general/path.h>
#include <general/stristr.h>
#include <iostream>
#include "io_functions.h"

#include "boost/filesystem/path.hpp"
#include "boost/filesystem/operations.hpp"
namespace fs = boost::filesystem;

using namespace std;

// Drop-in replacements for vcl routines.
std::string ExpandFileName(const char *s){
   fs::path p(s);
   fs::path q = system_complete(p);
   return(q.native_file_string());
}

bool FileExists (const std::string &f) {
   fs::path p(f);
   return (fs::is_empty(p));
}

bool DirectoryExists (const std::string &d) {
   fs::path p(d);
   return (fs::is_directory(p));
}

// ------------------------------------------------------------------
// Return a list of files/directories to caller.
// ------------------------------------------------------------------
void getDirectoryListing(const std::string& directoryName,
                         const std::string& extension,
                         std::vector<std::string>& dirList,
                         unsigned int attribute,
                         bool fullPath)
   {
   Path p;
   struct ffblk ffblk;
   int done;
   p.Set_path (directoryName.c_str());
   p.Set_name (extension.c_str());
   done = findfirst(p.Get_path().c_str(), &ffblk, attribute);
   while (!done)
      {
      bool NormalFile = ((ffblk.ff_attrib & FA_DIREC) == 0);
      bool Keep = (strcmp(ffblk.ff_name, ".")!=0 &&
                   strcmp(ffblk.ff_name, "..") != 0);

      if (attribute == 0)
         Keep = Keep && NormalFile;
      else
         Keep = Keep && ((ffblk.ff_attrib & attribute) > 0);

      if (Keep)
         {
         Path p;
         if (fullPath)
            p.Set_directory (directoryName.c_str());
         p.Set_name(ffblk.ff_name);
         dirList.push_back (p.Get_path());
         }
      done = findnext (&ffblk);
      }
   }

// ------------------------------------------------------------------
// Locate a file and return it's full absolute path name
// ------------------------------------------------------------------
std::string locateFile(std::vector<std::string>& searchDirectories,
                       const std::string& searchFile)
   {
   Path p;
   bool found = false;

   // loop through all directories.
   for (vector<string>::iterator Iter = searchDirectories.begin();
                                 Iter != searchDirectories.end() && !found;
                                 Iter++)
      {
      p.Set_directory ( (*Iter).c_str() );
      p.Set_name (searchFile.c_str());
      struct ffblk ffblk;
      found = (findfirst(p.Get_path().c_str(), &ffblk, FA_NORMAL) == false);
      }

   if (found)
      return p.Get_path();
   else
      return "";
   }

// ------------------------------------------------------------------
// Get a directory listing but make it recursive.
//      The max_num_levels_to_descend should be set to the number of
//      nested directories to descend into.  If set to some high number
//      (eg 10000) then all directories will be recursed into.  If set
//      to zero then no directories will be recursed into.  If
//      "include_specified_directory" is true then files in the specified
//      "directory" will be returned along with files in child directories.
// ------------------------------------------------------------------
void getRecursiveDirectoryListing(const std::string& directory,
                                  const std::string& fileSpec,
                                  int maxNumLevelsToDescend,
                                  bool includeSpecifiedDirectory,
                                  std::vector<std::string>& files)
   {
   // get matching files in this directory if necessary
   if (includeSpecifiedDirectory)
      getDirectoryListing(directory, fileSpec, files, FA_NORMAL, true);

   // get all sub directories.
   vector<string> Sub_dirs;
   getDirectoryListing(directory, "*.*", Sub_dirs, FA_DIREC, false);

   // loop through all sub directories and recurse through them if
   // we haven't reached the maximum number of nested levels.
   if (maxNumLevelsToDescend > 0)
      {
      maxNumLevelsToDescend--;
      for (vector<string>::iterator i = Sub_dirs.begin();
                                    i != Sub_dirs.end();
                                    i++)
         {
         string dir = directory;
         dir += "\\";
         dir += *i;
         getRecursiveDirectoryListing(dir.c_str(), fileSpec, maxNumLevelsToDescend, true, files);
         }
      }
   }

// ------------------------------------------------------------------
// Copy or move the specified source files to the destination directory
// ------------------------------------------------------------------
// XXX why use winapi when we could iterate over CopyFile?
void copyFiles(std::vector<std::string>& sourceFiles,
               const std::string& destinationDirectory,
               bool doMoveFiles)
   {
   string source_st;
   Build_string(sourceFiles, ";", source_st);
   source_st += ";";

   // convert source_st to double null terminated strings.
   char* source_string = new char[source_st.length() + 1];
   strcpy (source_string, source_st.c_str());
   Replace_all_chars (source_string, ';', '\0');

   // call Windows api routine
   SHFILEOPSTRUCT op;
   ZeroMemory(&op,sizeof(op));
   op.hwnd=GetForegroundWindow();
   if (doMoveFiles)
      op.wFunc = FO_MOVE;
   else
      op.wFunc=FO_COPY;
   op.pFrom=source_string;
   op.pTo=destinationDirectory.c_str();
   op.fFlags = FOF_NOCONFIRMMKDIR;
   SHFileOperation(&op);

   // cleanup
   delete [] source_string;
   }

// ------------------------------------------------------------------
// Copy or move the specified source directories and all directories below them
// to the destination directory.
// ------------------------------------------------------------------
void copyDirectories(std::vector<std::string>& sourceDirectories,
                     const std::string& destinationDirectory,
                     bool doMoveFiles)
   {
   string source_st, destination_st;

   // use the first directories parent as the base directory.  Assumes that
   // all directories are located under this parent.
   Path Home_directory;
   if (sourceDirectories.size() > 0)
      {
      Home_directory.Set_path ( (*sourceDirectories.begin()).c_str() );
      Home_directory.Back_up_directory();
      }

   // get a complete list of files we want to copy.
   vector<string> Files;
   for (vector<string>::iterator i = sourceDirectories.begin();
                                 i != sourceDirectories.end();
                                 i++)
      {
      string source_directory = *i;

      // get a list of all filenames
      vector<string> Files;
      getRecursiveDirectoryListing( (*i).c_str(), "*.*", 10000, true, Files);

      // create a semicolon delimited string to hold all file names.
      string st;
      Build_string(Files, ";", st);
      st += ";";

      // add filename string to source_st
      source_st += st;

      // replace all source directories with destination directories and add
      // to destination_st
      Replace_all (st, Home_directory.Get_directory().c_str(), destinationDirectory.c_str());
      destination_st += st;
      }

   // convert source_st and destination_st to double null terminated strings.
   char* source_string = new char[source_st.length() + 1];
   strcpy (source_string, source_st.c_str());
   Replace_all_chars (source_string, ';', '\0');

   char* destination_string = new char[destination_st.length() + 1];
   strcpy (destination_string, destination_st.c_str());
   Replace_all_chars (destination_string, ';', '\0');

   // call Windows api routine
   SHFILEOPSTRUCT op;
   ZeroMemory(&op,sizeof(op));
   op.hwnd=GetForegroundWindow();
   if (doMoveFiles)
      op.wFunc = FO_MOVE;
   else
      op.wFunc=FO_COPY;
   op.pFrom=source_string;
   op.pTo=destination_string;
   op.fFlags = FOF_MULTIDESTFILES + FOF_NOCONFIRMMKDIR;
   SHFileOperation(&op);

   // cleanup
   delete [] source_string;
   delete [] destination_string;
   }

// ------------------------------------------------------------------
// Copy or move the specified source files to the destination
// directory preserving the directory structure.
//     The source_base_directory is used to determine the root of all the
//     source files.  It is assumed that all source files are located
//     under this base directory.  Any that aren't are NOT copied.
//
//     Each source file has the "source_base_directory" part of its
//     path replaced with the destination directory. eg:
//        If           source file = c:\apsuite\apsim\chickpea\chickpea.apf
//           source_base_directory = c:\apsuite\apsim
//           destination_directory = c:\apswork
//        Then    destination file = c:\apswork\chickpea\chickpea.apf
// ------------------------------------------------------------------
void copyFilesPreserveDirectories(std::vector<std::string>& sourceFiles,
                                  const std::string& sourceBaseDirectory,
                                  const std::string& destinationDirectory,
                                  bool doMoveFiles,
                                  bool makeFilesReadWrite)
   {
   string source_st, destination_st;
   // loop through all source files and replace source_base_directory
   // with destination_directory.
   for (vector<string>::iterator file = sourceFiles.begin();
                                 file != sourceFiles.end();
                                 file++)
      {
      string filest = *file;
      if (stristr((char*) filest.c_str(), sourceBaseDirectory.c_str()) == filest.c_str())
         {
         filest.replace (0, strlen(sourceBaseDirectory.c_str()), destinationDirectory.c_str());
         source_st += (*file) + ";";
         destination_st += filest + ";";
         }
      }

   // convert source_st and destination_st to double null terminated strings.
   char* source_string = new char[source_st.length() + 1];
   strcpy (source_string, source_st.c_str());
   Replace_all_chars (source_string, ';', '\0');

   char* destination_string = new char[destination_st.length() + 1];
   strcpy (destination_string, destination_st.c_str());
   Replace_all_chars (destination_string, ';', '\0');

   // call Windows api routine
   SHFILEOPSTRUCT op;
   ZeroMemory(&op,sizeof(op));
   op.hwnd=GetForegroundWindow();
   if (doMoveFiles)
      op.wFunc = FO_MOVE;
   else
      op.wFunc=FO_COPY;
   op.pFrom=source_string;
   op.pTo=destination_string;
   op.fFlags = FOF_MULTIDESTFILES + FOF_NOCONFIRMMKDIR;
   SHFileOperation(&op);

   // if we need to make sure files are read/write then do so.
   if (makeFilesReadWrite)
      {
      vector<string> Destination_files;
      Split_string (destination_st, ";", Destination_files);
      for (vector<string>::iterator file = Destination_files.begin();
                                  file != Destination_files.end();
                                  file++)
         {
         SetFileAttributes ( (*file).c_str(), FILE_ATTRIBUTE_ARCHIVE);
         }
      }

   // cleanup
   delete [] source_string;
   delete [] destination_string;
   }

// ------------------------------------------------------------------
// Send the specified directory or files to the recycle bin.
// ------------------------------------------------------------------
void deleteFilesOrDirectories(std::vector<std::string>& directories)
   {
   // create a string to hold all filenames.
   string st;
   Build_string (directories, ";", st);
   st += ";";

   // convert string to a double null terminated string.
   char* source_string = new char[st.length() + 1];
   strcpy (source_string, st.c_str());
   Replace_all_chars (source_string, ';', '\0');

   // call Windows api routine
   SHFILEOPSTRUCT op;
   ZeroMemory(&op,sizeof(op));
   op.hwnd=GetForegroundWindow();
   op.wFunc=FO_DELETE;
   op.pFrom=source_string;
   op.fFlags = FOF_ALLOWUNDO;
   SHFileOperation(&op);

   // cleanup
   delete [] source_string;
   }
// ------------------------------------------------------------------
// Return the youngest file in a given directory that matches the
// specified filespec.
// ------------------------------------------------------------------
std::string getYoungestFile(const std::string& directory,
                            const std::string& filespec)
   {
   unsigned short Maximum_date = 0;
   unsigned short Maximum_time = 0;
   Path Youngest_file;


   struct ffblk ffblk;
   int done;
   Path p;
   p.Set_path (directory.c_str());
   p.Set_name (filespec.c_str());
   done = findfirst(p.Get_path().c_str(),&ffblk, FA_NORMAL);
   while (!done)
      {
      if (ffblk.ff_fdate > Maximum_date ||
         (ffblk.ff_fdate == Maximum_date && ffblk.ff_ftime > Maximum_time))
         {
         Youngest_file.Set_directory (directory.c_str());
         Youngest_file.Set_name (ffblk.ff_name);
         }
      done = findnext(&ffblk);
      }
   return Youngest_file.Get_path();
   }
// ------------------------------------------------------------------
// Rename the specified file or folder if there is a name collision.
// ------------------------------------------------------------------
void renameOnCollision(std::string& name, bool isFile)
   {
   int collisionIndex = 2;
   std::string newName = name;
   while ((isFile && FileExists(newName)) || (!isFile && DirectoryExists(newName)))
      {
      char buf[40];
      newName = name + " (" + itoa(collisionIndex, buf, 10) + ")";
      collisionIndex++;
      }
   name = newName;
   }

