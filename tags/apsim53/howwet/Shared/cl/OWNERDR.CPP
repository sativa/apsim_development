#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include <cl\ownerdr.h>

// *******************************************************************
      GOwner_draw_base::GOwner_draw_base(void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Selected_item = 0;
   }

// *******************************************************************
       GOwner_draw_combo::GOwner_draw_combo (TWindow*           Parent,
                                             int                resID,
                                             TModule*           module,
                                             GOwner_draw_base*  Owner_draw_p)
         : TComboBox(Parent, resID, 0, module)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Owner_draw_ptr = Owner_draw_p;
   }

// *******************************************************************
       void GOwner_draw_combo::SetupWindow(void)  {
// *******************************************************************

//  Short description:
//    setup combo box.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   TComboBox::SetupWindow();

   for (int Element = 0;
        Element < Owner_draw_ptr->Get_num_items();
        Element++)
      {
      AddString("");
      }

   SetSelIndex(Owner_draw_ptr->Get_selected_item());
   }

// *******************************************************************
       void GOwner_draw_combo::DrawItem(DRAWITEMSTRUCT far &Draw_info)  {
// *******************************************************************

//  Short description:
//    draw item in box

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Owner_draw_ptr->DrawItem(Draw_info);
   }

// *******************************************************************
       bool GOwner_draw_combo::CanClose(void)  {
// *******************************************************************

//  Short description:
//    about to close - save current selection.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Owner_draw_ptr->Set_selected_item(GetSelIndex());

   return TRUE;
   }

// *******************************************************************
       GOwner_draw_list::GOwner_draw_list (TWindow*           Parent,
                                           int                resID,
                                           TModule*           module,
                                           GOwner_draw_base*  Owner_draw_p)
         : TListBox(Parent, resID, module)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Owner_draw_ptr = Owner_draw_p;
   }

// *******************************************************************
       void GOwner_draw_list::SetupWindow(void)  {
// *******************************************************************

//  Short description:
//    setup listbox box.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   TListBox::SetupWindow();

   for (int Element = 0;
        Element < Owner_draw_ptr->Get_num_items();
        Element++)
      {
      AddString("");
      }

   SetSelIndex(Owner_draw_ptr->Get_selected_item());
   }

// *******************************************************************
       void GOwner_draw_list::DrawItem(DRAWITEMSTRUCT far &Draw_info)  {
// *******************************************************************

//  Short description:
//    draw item in box

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Owner_draw_ptr->DrawItem(Draw_info);
   }

// *******************************************************************
       bool GOwner_draw_list::CanClose(void)  {
// *******************************************************************

//  Short description:
//    about to close - save current selection.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Owner_draw_ptr->Set_selected_item(GetSelIndex());

   return TRUE;
   }


