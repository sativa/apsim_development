#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include <graph\objlist.h>
#include <stdio.h>

IMPLEMENT_CASTABLE (Object_list);
IMPLEMENT_OUR_STREAMABLE (Object_list);

Object_list::Object_list (StreamableInit)
   : Objects(10, 0, 10), Object_names(10, 0, 10)
    {};

void Set_coord_for_each (Drawable_object& Obj, void *Coord_void)
   {
   Obj.Set_coord(*((Coordinate_system*) Coord_void));
   }
void Paint_for_each (Drawable_object& Obj, void *DC_void)
   {
   Obj.Paint(*((GDC*) DC_void));
   }

// *******************************************************************
      void Object_list::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of Object_list to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   os << GetObject()->Objects.GetItemsInContainer();
   for (int Object_number = 0;
        Object_number < GetObject()->Objects.GetItemsInContainer();
        Object_number++)
      {
      os << GetObject()->Objects[Object_number];
      os << GetObject()->Object_names[Object_number];
      }
   }

// *******************************************************************
      void *Object_list::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of Object_list from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
      Drawable_object *Object_ptr;     // pointer to object.
      GString Object_name;             // name of object.

// -------------------- Executable code section ----------------------

   GetObject()->Delete_all();

   int Num_objects;
   is >> Num_objects;
   for (int Object_number = 0;
        Object_number < Num_objects;
        Object_number++)
      {
      is >> Object_ptr;
      is >> Object_name;
      GetObject()->Add(Object_ptr,
                       Front,
                       Object_name.c_str());
      }

   return GetObject();
   }

// *******************************************************************
       Object_list::Object_list(void)
      : Objects(10, 0, 10), Object_names(10, 0, 10)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   // we don't own elements.

   Selected_index = -1;
   Rect_drag.Set_dragging_on(FALSE);
   DC_ptr = NULL;
   }

// *******************************************************************
      void Object_list::Add (Drawable_object* Draw_object_ptr,
                             Front_back_enum Front_back,
                             const char* Object_name)  {
// *******************************************************************

//  Short description:
//    Add an object into the system.

//  Notes:

//  Changes:
//    DPH 17/1/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   if (Front_back == Front)
      {
      Objects.Add(Draw_object_ptr);
      Object_names.Add(GString(Object_name));
      }

   else
      {
      Objects.AddAt(Draw_object_ptr, 2);
      Object_names.AddAt(GString(Object_name), 2);
      }
   }

// *******************************************************************
       void Object_list::Delete_all(void)  {
// *******************************************************************

//  Short description:
//    Delete all objects.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Objects.Flush();
   Object_names.Flush();
   Selected_index = -1;
   }

// *******************************************************************
       void Object_list::Delete(const char* Object_name)  {
// *******************************************************************

//  Short description:
//    Delete specified object.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   int Delete_index = Object_names.Find(Object_name);
   if (Delete_index >= 0)
      {
      Objects.Destroy(Delete_index);
      Object_names.Destroy(Delete_index);
      }
   }

// *******************************************************************
       void Object_list::Check_series_numbering(const char* Series_deleted)  {
// *******************************************************************

//  Short description:
//    Check series numbering.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
      char St[20];                     // object name for series.
      int Series_index;

// -------------------- Executable code section ----------------------

   // work out the number of the series just deleted.

   int Series_number_deleted;
   sscanf(Series_deleted, "%*6c%i", &Series_number_deleted);

   // loop through all series starting with the series after the deleted one.

   int Series_number = Series_number_deleted + 1;
   sprintf (St, "series%i", Series_number);
   Series_index = Object_names.Find(GString(St));

   while (Series_index != INT_MAX)
      {
      // renumber this series.  Subtract 1 from it.

      sprintf (St, "series%i", Series_number - 1);
      Object_names[Series_index] = St;

      // goto next series.

      Series_number++;
      sprintf (St, "series%i", Series_number);
      Series_index = Object_names.Find(GString(St));
      }
   }

// *******************************************************************
       void Object_list::Delete_current_object(void)  {
// *******************************************************************

//  Short description:
//    Delete currently selected object.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
      GString Object_name_deleted;     // name of object just deleted.

// -------------------- Executable code section ----------------------

   if (Selected_index >= 0)
      {
      Object_name_deleted = Object_names[Selected_index];
      Objects.Destroy(Selected_index);
      Object_names.Destroy(Selected_index);
      Selected_index = -1;
      Check_series_numbering(Object_name_deleted.c_str());
      }
   }


// *******************************************************************
       void Object_list::Set_coord(Coordinate_system& Coord)  {
// *******************************************************************

//  Short description:
//    set the coordinate system of all objects.

//  Notes:

//  Changes:
//    DPH 17/1/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Objects.ForEach(Set_coord_for_each, &Coord);
   }

// *******************************************************************
       int Object_list::Find_object(GDC& DC, TPoint& Point)  {
// *******************************************************************

//  Short description:
//    Find the object that point lies over.  Return the object index
//    when found.

//  Notes:

//  Changes:
//    DPH 17/1/95

//  Calls:

//  Internal variables
      bool Found = FALSE;              // Found the graphic object yet?
      int Index;

// -------------------- Executable code section ----------------------

   // call all objects hit_test routine until one returns TRUE.
   // scan the list backwards.

   for (Index = Objects.GetItemsInContainer() - 1;
        Index >= 0 && !Found;
        Index--)
      {
      Found = Objects[Index]->Hit_test(DC, Point);
      }
   Index++;

   return Index;
   }

// *******************************************************************
       void Object_list::Select(int Select_index, bool Fix_screen)  {
// *******************************************************************

//  Short description:
//    Select the specified item.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   // remove old selection.

   if (Selected_index >= 0 && Fix_screen)
      {
      Objects[Selected_index]->Set_selected(FALSE);
      Objects[Selected_index]->Draw_selection(*DC_ptr);
      }

   // select new item.

   if (Select_index >= 0 && Fix_screen)
      {
      Objects[Select_index]->Set_selected(TRUE);
      Objects[Select_index]->Draw_selection(*DC_ptr);
      }

   Selected_index = Select_index;
   }

// *******************************************************************
       void Object_list::Left_button_down(TWindow& Parent, TPoint& Point)  {
// *******************************************************************

//  Short description:
//    The left button was clicked.  Process event.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
      TRect r;                         // Bounding rectangle for current object
      int New_selection;               // Newly selected object.
      bool Remove_old_selection;       // Remove old selection?
      bool Draw_new_selection;         // draw new selection?

// -------------------- Executable code section ----------------------

   DC_ptr = new GDC(GetDC(Parent));

   // Get object mouse is over.  If the mouse is over a selection
   // handle then no need to look for object as the currently
   // selected object is still valid.

   if (Rect_drag.Get_mouse_over_point())
      New_selection = Selected_index;

   else
      New_selection = Find_object(*DC_ptr, Point);

   // Check to see if user has clicked on the same object.

   if (Selected_index == New_selection)
      {
      // Is this object movable or sizable.

      if (Objects[Selected_index]->Get_sizable() ||
          Objects[Selected_index]->Get_movable())
         {
         Remove_old_selection = FALSE;
         Draw_new_selection = FALSE;
         Rect_drag.Left_button_down(Parent, Point);
         }

      else
         {
         Remove_old_selection = TRUE;
         Draw_new_selection = FALSE;
         }
      }
   else
      {
      Remove_old_selection = TRUE;
      Draw_new_selection = TRUE;
      }

   // set the ROP flag of DC.

   DC_ptr->SetROP2(R2_XORPEN);

   // remove old selection if necessary.

   if (Remove_old_selection && Selected_index != -1)
      {
      Objects[Selected_index]->Set_selected(FALSE);
      Objects[Selected_index]->Draw_selection(*DC_ptr);
      Selected_index = -1;
      }

   // draw new selection if necessary.

   if (Draw_new_selection)
      {
      Selected_index = New_selection;
      Objects[Selected_index]->Set_selected(TRUE);
      Objects[Selected_index]->Draw_selection(*DC_ptr);

      // Is object sizable or movable?

      if (Objects[Selected_index]->Get_sizable() ||
          Objects[Selected_index]->Get_movable())
         {

         // Setup the drag object for currently selected object.

         Rect_drag.Set_flags(Objects[Selected_index]->Get_movable(),
                             Objects[Selected_index]->Get_sizable());
         Rect_drag.Set_dragging_on(TRUE);
         Objects[Selected_index]->Get_bounding_rectangle(*DC_ptr, r);
         if (r.left != r.right)
            Rect_drag.Set_points(r);
         else
            Rect_drag.Set_dragging_on(FALSE);
         }
      }
   }

// *******************************************************************
       void Object_list::Left_button_up(TPoint& Point)  {
// *******************************************************************

//  Short description:
//    The left button was released.  Process event.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
      TRect r;                         // Bounding rectangle for current object

// -------------------- Executable code section ----------------------

   if (Rect_drag.Get_is_dragging())
      {
      Rect_drag.Left_button_up(Point);
      Rect_drag.Get_points(r);
      Objects[Selected_index]->Set_bounding_rectangle(*DC_ptr, r);

      // Invalidate parent so that it will be redrawn.

      ::InvalidateRect(GetFocus(), NULL, TRUE);
      }

   if (DC_ptr != NULL)
      {
      delete DC_ptr;
      DC_ptr = NULL;
      }
   }

// *******************************************************************
       void Object_list::Mouse_move(TPoint& Point)  {
// *******************************************************************

//  Short description:
//    The mouse was moved.  Process event.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   if (Rect_drag.Get_dragging_on())
      Rect_drag.Mouse_move(Point);
   }

// *******************************************************************
       void Object_list::Right_button_down(TWindow& Parent, TPoint& Point)  {
// *******************************************************************

//  Short description:
//    The right button was clicked.  Process event.  Return TRUE if event was
//    used.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
      TPopupMenu Popup_menu;           // our popup menu.

// -------------------- Executable code section ----------------------

   if (Selected_index >= 0)
      {

      // Create menu for this object.

      int Menu_id = MENU_START_ID;
      Objects[Selected_index]->Create_popup_menu(Popup_menu, Menu_id);

      // Execute menu for this object.

      Parent.ClientToScreen(Point);
      Popup_menu.TrackPopupMenu(TPM_LEFTALIGN, Point,  0, Parent);
      }
   }

// *******************************************************************
       void Object_list::Command(TWindow& Parent, int id)  {
// *******************************************************************

//  Short description:
//    The user clicked on a popup menu.  Go perform required action.
//    Return TRUE if object owns Id.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   if (Selected_index >= 0)
      {
      int Menu_id = MENU_START_ID;
      Objects[Selected_index]->Do_menu_item(Parent, Menu_id, id);
      }
   }

// *******************************************************************
       void Object_list::Paint(GDC& DC)  {
// *******************************************************************

//  Short description:
//    Paint all objects.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   // call all objects paint methods

   Objects.ForEach(Paint_for_each, &DC);

   // Select the current object.

   if (Selected_index >= 0)
      {
      DC.SetROP2(R2_XORPEN);
      Objects[Selected_index]->Draw_selection(DC);
      DC.SetROP2(R2_COPYPEN);
      }
   }

