#include "io_functions.h"
#include <general\path.h>
#include <dir.h>
// ------------------------------------------------------------------
//  Short description:
//      return a list of files/directories to caller.

//  Notes:

//  Changes:
//    DPH 11/6/1997

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_directory_listing (const char* Directory_name,
                                           const char* Extension,
                                           list<string>& Dir_list,
                                           unsigned int Attribute,
                                           bool Full_path)
   {
   Path p;

   struct ffblk ffblk;
   int done;
   p.Set_path (Directory_name);
   p.Set_name (Extension);
   done = findfirst(p.Get_path().c_str(),&ffblk, Attribute);
   while (!done)
      {
      if (strcmpi(ffblk.ff_name, ".") != 0 &&
          strcmpi(ffblk.ff_name, "..") != 0 &&
          (ffblk.ff_attrib & Attribute) == Attribute)
         {
         Path p (ffblk.ff_name);
         if (Full_path)
            p.Set_directory (Directory_name);
         Dir_list.push_back (p.Get_path());
         }
      done = findnext(&ffblk);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      locate a file and return it's full absolute path name

//  Notes:
//      search file passed in should not contain any directory info.

//  Changes:
//    DPH 11/6/1997

// ------------------------------------------------------------------
string GENERAL_EXPORT Locate_file (list<string>& Search_directories,
                                   const char* Search_file)
   {
   Path p;
   bool found = false;

   // loop through all directories.
   for (list<string>::iterator Iter = Search_directories.begin();
                               Iter != Search_directories.end() && !found;
                               Iter++)
      {
      p.Set_directory ( (*Iter).c_str() );
      p.Set_name (Search_file);
      struct ffblk ffblk;
      found = (findfirst(p.Get_path().c_str(), &ffblk, FA_NORMAL) == false);
      }

   if (found)
      return p.Get_path();
   else
      return "";
   }

// ------------------------------------------------------------------
//  Short description:
//      get a directory listing but make it recursive.

//  Notes:
//      The max_num_levels_to_descend should be set to the number of
//      nested directories to descend into.  If set to some high number
//      (eg 10000) then all directories will be recursed into.  If set
//      to zero then no directories will be recursed into.

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_recursive_directory_listing(const char* Directory,
                                                    const char* File_spec,
                                                    int Max_num_levels_to_descend,
                                                    list<string>& Files)
   {
   // get matching files in this directory.
   Get_directory_listing (Directory,
                          File_spec,
                          Files,
                          FA_NORMAL,
                          true);

   // get all sub directories.
   list<string> Sub_dirs;
   Get_directory_listing (Directory,
                          "*.*",
                          Sub_dirs,
                          FA_DIREC,
                          false);

   // loop through all sub directories and recurse through them if
   // we haven't reached the maximum number of nested levels.
   if (Max_num_levels_to_descend > 0)
      {
      Max_num_levels_to_descend--;
      for (list<string>::iterator i = Sub_dirs.begin();
                                  i != Sub_dirs.end();
                                  i++)
         {
         string dir = Directory;
         dir += "\\";
         dir += *i;
         Get_recursive_directory_listing (dir.c_str(), File_spec, Max_num_levels_to_descend, Files);
         }
      }
   }

