/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         so_what.cpp
    AUTHOR:


    OVERVIEW
    ========
    Source file for implementation of So_what_dlg (TDialog).
*/

#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>

#include "so_what.h"
#include <consts.rh>
#include <gobjs\srchstrm.h>

//{{So_what_dlg Implementation}}


So_what_dlg::So_what_dlg (TWindow* parent, int Screen_num, int Page_num,
                          TResId resId, TModule* module):
    TDialog(parent, resId, module)
{
    // INSERT>> Your constructor code here.

    Screen_number = Screen_num;
    Page_number = Page_num;
    List_box_ptr = new View_box(this, IDC_LISTBOX1);
}


So_what_dlg::~So_what_dlg ()
{
    Destroy();

    // INSERT>> Your destructor code here.

}


// *******************************************************************
      void So_what_dlg::SetupWindow ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
      GString Search_string;           // String to search for.
      GString Number;                  // string version of number

// -------------------- Executable code section ----------------------


    TDialog::SetupWindow();

   // INSERT>> Your code here.

   // Setup help search string

   Search_string = "//";
   Number.Set_integer(Screen_number);
   Search_string += Number;
   Search_string += "/";
   Number.Set_integer(Page_number);
   Search_string += Number;
   Search_string += "/";

   // Go find search string in help file

   ifstream In_stream ("howwet.hlp", ios::binary);
   bool Found = Search_stream(In_stream, Search_string.c_str());

   // Advance stream past the matched string

   In_stream.seekg(Search_string.length(), ios::cur);

   // If found then go fill view box.

   if (Found)
      {
      List_box_ptr->Read_from_stream(In_stream, "//");
      }
   }

