#include <graph\global.h>
#pragma hdrstop

#ifdef INTERACTIVE
// *******************************************************************
      void Drawable_object::Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id)  {
// *******************************************************************

//  Short description:
//    Create the popup menu for this object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Popup_menu.AppendMenu(MF_ENABLED, Menu_id, "&Pen properties");
   Popup_menu.AppendMenu(MF_ENABLED, Menu_id+1, "&Brush properties");
   Menu_id += 2;
   }

// *******************************************************************
      bool Drawable_object::Do_menu_item
         (TWindow& Parent, int& Menu_id, int User_selected_id)  {
// *******************************************************************

//  Short description:
//    If the user_selected_id belongs to us then return TRUE and perform
//    appropriate actions.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      bool ID_is_ours = TRUE;          // Is Id ours?

// -------------------- Executable code section ----------------------

   if (User_selected_id == Menu_id)
      Pen.Properties(Parent);

   else if (User_selected_id == Menu_id + 1)
      Brush.Properties(Parent);

   else
      ID_is_ours = FALSE;

   Menu_id += 2;

   return ID_is_ours;
   }
#endif
