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
                            unsigned int Attribute = FA_NORMAL);

#endif
