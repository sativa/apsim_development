#include <vcl.h>
#pragma hdrstop
#include "io_functions.h"
#include <general\path.h>
#include <dos.h>
#include <dir.h>
#include <shellapi.h>
// ------------------------------------------------------------------
//  Short description:
//      return a list of files/directories to caller.

//  Notes:

//  Changes:
//    DPH 11/6/1997
//    dph 11/1/1998 modified to use VCL findfirst routines.

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_directory_listing (const char* Directory_name,
                                           const char* Extension,
                                           list<string>& Dir_list,
                                           unsigned int Attribute,
                                           bool Full_path)
   {
   if (Attribute == 0)
      Attribute = faAnyFile;

   Path p;

   TSearchRec SearchRec;
   int done;
   p.Set_path (Directory_name);
   p.Set_name (Extension);
   done = FindFirst(p.Get_path().c_str(), Attribute, SearchRec);
   while (!done)
      {
      bool NormalFile = ((SearchRec.Attr & faDirectory) == 0);
      bool Keep = (SearchRec.Name != "." && SearchRec.Name != "..");

      if (Attribute == faAnyFile)
         Keep = Keep && NormalFile;

      else
         Keep = Keep && ((SearchRec.Attr & Attribute) > 0);

      if (Keep)
         {
         Path p;
         if (Full_path)
            p.Set_directory (Directory_name);
         p.Set_name(SearchRec.Name.c_str());
         Dir_list.push_back (p.Get_path());
         }
      done = FindNext (SearchRec);
      }
   FindClose(SearchRec);
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
                                                    bool Include_specified_directory,
                                                    list<string>& Files)
   {
   // get matching files in this directory if necessary
   if (Include_specified_directory)
      {
      Get_directory_listing (Directory,
                             File_spec,
                             Files,
                             FA_NORMAL,
                             true);
      }

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
         Get_recursive_directory_listing (dir.c_str(), File_spec, Max_num_levels_to_descend, true, Files);
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     copy or move the specified source files to the destination directory

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Copy_files (list<string>& Source_files,
                                const char* Destination_directory,
                                bool Do_move_files)
   {
   string source_st;
   Build_string(Source_files, ";", source_st);
   source_st += ";";

   // convert source_st to double null terminated strings.
   char* source_string = new char[source_st.length() + 1];
   strcpy (source_string, source_st.c_str());
   Replace_all_chars (source_string, ';', '\0');

   // call Windows api routine
   SHFILEOPSTRUCT op;
   ZeroMemory(&op,sizeof(op));
   op.hwnd=GetForegroundWindow();
   if (Do_move_files)
      op.wFunc = FO_MOVE;
   else
      op.wFunc=FO_COPY;
   op.pFrom=source_string;
   op.pTo=Destination_directory;
   op.fFlags = FOF_NOCONFIRMMKDIR;
   SHFileOperation(&op);

   // cleanup
   delete [] source_string;
   }

// ------------------------------------------------------------------
//  Short description:
//     copy or move the specified source directories and all directories below them
//     to the destination directory.

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Copy_directories (list<string>& Source_directories,
                                      const char* Destination_directory,
                                      bool Do_move_files)
   {
   string source_st, destination_st;

   // use the first directories parent as the base directory.  Assumes that
   // all directories are located under this parent.
   Path Home_directory;
   if (Source_directories.size() > 0)
      {
      Home_directory.Set_path ( (*Source_directories.begin()).c_str() );
      Home_directory.Back_up_directory();
      }

   // get a complete list of files we want to copy.
   list<string> Files;
   for (list<string>::iterator i = Source_directories.begin();
                               i != Source_directories.end();
                               i++)
      {
      string source_directory = *i;

      // get a list of all filenames
      list<string> Files;
      Get_recursive_directory_listing( (*i).c_str(), "*.*", 10000, true, Files);

      // create a semicolon delimited string to hold all file names.
      string st;
      Build_string(Files, ";", st);
      st += ";";

      // add filename string to source_st
      source_st += st;

      // replace all source directories with destination directories and add
      // to destination_st
      Replace_all (st, Home_directory.Get_directory().c_str(), Destination_directory);
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
   if (Do_move_files)
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
//  Short description:
//     send the specified directory and all directories below it to the
//     recycle bin.

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Delete_directories (list<string>& Directories)
   {
   // create a string to hold all filenames.
   string st;
   Build_string (Directories, ";", st);
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
   op.fFlags = FOF_NOCONFIRMATION + FOF_ALLOWUNDO;
   SHFileOperation(&op);

   // cleanup
   delete [] source_string;
   }

// ------------------------------------------------------------------
//  Short description:
//     return the youngest file in a given directory that matches the
//     specified filespec.

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
string GENERAL_EXPORT Get_youngest_file (const char* Directory,
                                         const char* Filespec)
   {
   unsigned short Maximum_date = 0;
   unsigned short Maximum_time = 0;
   Path Youngest_file;


   struct ffblk ffblk;
   int done;
   Path p;
   p.Set_path (Directory);
   p.Set_name (Filespec);
   done = findfirst(p.Get_path().c_str(),&ffblk, FA_NORMAL);
   while (!done)
      {
      if (ffblk.ff_fdate > Maximum_date ||
         (ffblk.ff_fdate == Maximum_date && ffblk.ff_ftime > Maximum_time))
         {
         Youngest_file.Set_directory (Directory);
         Youngest_file.Set_name (ffblk.ff_name);
         }
      done = findnext(&ffblk);
      }
   return Youngest_file.Get_path();
   }

