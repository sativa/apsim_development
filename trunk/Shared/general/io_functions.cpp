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
void Get_directory_listing (const char* Directory_name,
                            const char* Extension,
                            list<string>& Dir_list,
                            unsigned int Attribute)
   {
   Path p;
   Dir_list.erase(Dir_list.begin(), Dir_list.end());

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
         Dir_list.push_back (ffblk.ff_name);
      done = findnext(&ffblk);
      }
   }

