//---------------------------------------------------------------------------
#ifndef io functionsH
#define io functionsH

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
void Get_directory_listing (const char* Directory_name,
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
string Locate_file (list<string>& Search_directories,
                    const char* Search_file);


#endif
