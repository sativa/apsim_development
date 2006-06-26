#include <graph\global.h>
#pragma hdrstop

#include <graph\markers.h>
#include <assert.h>

// *******************************************************************
      GMarkers::GMarkers(void)  {
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

   Set_marker_styles(All_markers_list, MAX_MARKERS);
   }

// *******************************************************************
      void* GMarkers::Get_item (int Item_id)  {
// *******************************************************************

//  Short description:
//    return pointer to item to caller.

//  Notes:

//  Changes:
//      DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   return &Marker_list[Item_id];
   }

// *******************************************************************
      void GMarkers::Remove_item (int Item_id)  {
// *******************************************************************

//  Short description:
//    remove item from list.

//  Notes:

//  Changes:
//      DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   // move all items in list from Item_id up 1 position.

   for (int Item_index = Item_id + 1;
        Item_index < Num_items;
        Item_index++)
      Marker_list[Item_index - 1] = Marker_list[Item_index];

   Num_items--;
   assert (Num_items >= 0);
   }

// *******************************************************************
      void GMarkers::Add_item (void* Data_ptr)  {
// *******************************************************************

//  Short description:
//    add item from list.

//  Notes:

//  Changes:
//      DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Num_items++;
   assert (Num_items < MAX_MARKERS);

   Marker_type_enum *Int_ptr = (Marker_type_enum*) Data_ptr;
   Marker_list[Num_items - 1] = *Int_ptr;
   }

// *******************************************************************
      void GMarkers::DrawItem(DRAWITEMSTRUCT far &Draw_info)  {
// *******************************************************************

//  Short description:
//    Draw item

//  Notes:

//  Changes:
//      DPH 8/8/95

//  Calls:

//  Internal variables
   TBrush *Brush;                      // Brush to use
   TPen *Pen;                          // Pen to use

// -------------------- Executable code section ----------------------

   if (Draw_info.itemID != 0xffff)
      {
      // Create device context object for screen

      GDC Screen_dc(Draw_info.hDC);

      // Create and select the pen and brush to use.

      Brush = new TBrush(TColor::White);

      if ((Draw_info.itemState & ODS_SELECTED) == ODS_SELECTED)
         Pen = new TPen(TColor::Black, 2);
      else
         Pen = new TPen(TColor::White, 2);

      Screen_dc.SelectObject(*Pen);
      Screen_dc.SelectObject(*Brush);
      GPoint p1;
      p1.Set_pixel(Draw_info.rcItem.left, Draw_info.rcItem.top);
      GPoint p2;
      p2.Set_pixel(Draw_info.rcItem.right, Draw_info.rcItem.bottom);
      Screen_dc.Rectangle(p1, p2);
      delete Pen;

      // Draw our rectangle of colour.

      Pen = new TPen(TColor::Black, 1);
      Screen_dc.SelectObject(*Pen);
      GPoint Point;

      Point.Set_pixel((Draw_info.rcItem.right + Draw_info.rcItem.left) / 2,
                      (Draw_info.rcItem.top + Draw_info.rcItem.bottom) / 2);

      int Item_id = Draw_info.itemID;

      if (Item_id == M_none)
         {
         Screen_dc.SetTextAlign(TA_CENTER | TA_TOP);
         Screen_dc.TextOut((Draw_info.rcItem.left + Draw_info.rcItem.right)/2,
                            Draw_info.rcItem.top, "None");
         }
      else
         {
         DMarker Marker;
         Marker.Set_marker_type(Marker_list[(Marker_type_enum) Draw_info.itemID]);
         Marker.XYPoint.Set_pixel ((Draw_info.rcItem.left + Draw_info.rcItem.right) / 2,
                                   (Draw_info.rcItem.top + Draw_info.rcItem.bottom) / 2);

         Marker.Paint(Screen_dc);
         }

      // Delete our GDI objects.

      delete Pen;
      delete Brush;
      }
   }

// *******************************************************************
      void GMarkers::Get_marker_styles(Marker_type_enum Markers[],
                                       int& Num_markers)  {
// *******************************************************************

//  Short description:
//    get the marker style list.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   for (int Index = 0;
        Index < Num_items;
        Index++)
      Markers[Index] = Marker_list[Index];

   Num_markers = Num_items;
   }

// *******************************************************************
      void GMarkers::Set_marker_styles(Marker_type_enum Markers[],
                                       int Num_markers)  {
// *******************************************************************

//  Short description:
//    set the list.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   for (int Index = 0;
        Index < Num_markers;
        Index++)
      Marker_list[Index] = Markers[Index];

   Num_items = Num_markers;
   }

// *******************************************************************
      void GMarkers::Set_marker_styles_invert(Marker_type_enum Markers[],
                                              int Num_markers)  {
// *******************************************************************

//  Short description:
//    set the list to the invert of the specified list.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Num_items = 0;

   // Loop through the all pen styles list

   for (int Index1 = 0;
        Index1 < MAX_MARKERS;
        Index1++)
      {
      // loop through the pen styles list passed in.

      bool Found = FALSE;
      for (int Index2 = 0;
           Index2 < Num_markers && !Found;
           Index2++)
         {
         Found = (All_markers_list[Index1] == Markers[Index2]);
         }


      if (!Found)
         {
         Num_items++;
         Marker_list[Num_items - 1] = All_markers_list[Index1];
         }
      }
   }

// *******************************************************************
      void GMarkers::Set_selection(Marker_type_enum& Marker_type)  {
// *******************************************************************

//  Short description:
//    set the selection.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   bool Found = FALSE;
   int Index;
   for (Index = 0;
        Index < Num_items && !Found;
        Index++)
      Found = (Marker_list[Index] == Marker_type);
   Index--;

   if (Found)
      Selected_item = Index;
   else
      assert (FALSE);

   }

