/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         clmt_dlg.cpp
    AUTHOR:       


    OVERVIEW
    ========
    Source file for implementation of Climate_dlg (TDialog).      
*/

#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>

#include "clmt_dlg.h"
#include <consts.rh>

//
// Build a response table for all messages/commands handled
// by the application.
//
DEFINE_RESPONSE_TABLE1(Climate_dlg, TDialog)
//{{Climate_dlgRSP_TBL_BEGIN}}
    EV_BN_CLICKED(IDOK, Ok_button),
    EV_WM_LBUTTONDBLCLK,
//{{Climate_dlgRSP_TBL_END}}
END_RESPONSE_TABLE;


//{{Climate_dlg Implementation}}


Climate_dlg::Climate_dlg (TWindow* parent, Parameters *Param_p, TResId resId, TModule* module):
    TDialog(parent, resId, module)
   {
   // INSERT>> Your constructor code here.

   Site_list_control = new TListBox(this, IDC_LISTBOX1);
   Param_ptr = Param_p;
   }


Climate_dlg::~Climate_dlg ()
{
    Destroy();

    // INSERT>> Your destructor code here.

}


// *******************************************************************
      void Climate_dlg::SetupWindow ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
      String_array *Site_list;         // site list

// -------------------- Executable code section ----------------------


    TDialog::SetupWindow();

   // INSERT>> Your code here.

   Param_ptr->Get_site_list(&Site_list);

   for (int Indx = 0;
        Indx < Site_list->GetItemsInContainer();
        Indx++)
      {
      Site_list_control->AddString((*Site_list)[Indx].c_str());
      }

   Site_list_control->SetSelIndex(Param_ptr->Get_current_site());
   }


// *******************************************************************
      void Climate_dlg::Ok_button ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   Param_ptr->Set_current_site(Site_list_control->GetSelIndex());
   CloseWindow();
   }


void Climate_dlg::EvLButtonDblClk (uint modKeys, TPoint& point)
{
    TDialog::EvLButtonDblClk(modKeys, point);

    // INSERT>> Your code here.

    CloseWindow(IDOK);
}

