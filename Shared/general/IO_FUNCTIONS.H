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
//      to zero then no directories will be recursed into.

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_recursive_directory_listing(const char* Directory,
                                                    const char* File_spec,
                                                    int Max_num_levels_to_descend,
                                                    list<string>& Files);

#endif
