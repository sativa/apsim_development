//---------------------------------------------------------------------------
#ifndef io functionsH
#define io functionsH

#include <general\general.h>
#include <general\string_functions.h>
#include <general\mylist.h>
#include <dos.h>
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
                                           unsigned int Attribute = FA_NORMAL,
                                           bool Full_path = false);

// ------------------------------------------------------------------
//  Short description:
//      locate a file and return it's full absolute path name

//  Notes:
//      search file passed in should not contain any directory info.

//  Changes:
//    DPH 11/6/1997

// ------------------------------------------------------------------
string GENERAL_EXPORT Locate_file (list<string>& Search_directories,
                                   const char* Search_file);

// ------------------------------------------------------------------
//  Short description:
//      get a directory listing but make it recursive.

//  Notes:
//      The max_num_levels_to_descend should be set to the number of
//      nested directories to descend into.  If set to some high number
//      (eg 10000) then all directories will be recursed into.  If set
//      to zero then no directories will be recursed into.  If
//      "include_specified_directory" is true then files in the specified
//      "directory" will be returned along with files in child directories.

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_recursive_directory_listing(const char* Directory,
                                                    const char* File_spec,
                                                    int Max_num_levels_to_descend,
                                                    bool Include_specified_directory,
                                                    list<string>& Files);

// ------------------------------------------------------------------
//  Short description:
//     copy or move the specified source files to the destination directory

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Copy_files (list<string>& Source_files,
                                const char* Destination_directory,
                                bool Do_move_files);

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
                                      bool Do_move_files = false);

// ------------------------------------------------------------------
//  Short description:
//     send the specified directory and all directories below it to the
//     recycle bin.

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Delete_directories (list<string>& Directories);

// ------------------------------------------------------------------
//  Short description:
//     return the youngest file in a given directory that matches the
//     specified filespec.

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
string GENERAL_EXPORT Get_youngest_file (const char* Directory,
                                         const char* Filespec);

#endif
