#include <graph\global.h>
#pragma hdrstop

#include <graph\penwidts.h>
#include <assert.h>

// *******************************************************************
      GPen_width_owner_draw::GPen_width_owner_draw(void)  {
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

   for (int Element = 0;
        Element < MAX_PEN_WIDTHS;
        Element++)
      {
      Pen_width_list[Element] = Element;
      }

   Num_items = MAX_PEN_WIDTHS;
   }

// *******************************************************************
      void GPen_width_owner_draw::Set_pen_widths
         (int Pen_widths[], int Num_pen_widths)  {
// *******************************************************************

//  Short description:
//    set the pen width list box.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   for (int i = 0;
        i < Num_pen_widths;
        i++)
      Pen_width_list[i] = Pen_widths[i];
   Num_items = Num_pen_widths;
   }

// *******************************************************************
      void* GPen_width_owner_draw::Get_item (int Item_id)  {
// *******************************************************************

//  Short description:
//    return pointer to item to caller.

//  Notes:

//  Changes:
//      DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   return &Pen_width_list[Item_id];
   }

// *******************************************************************
      void GPen_width_owner_draw::Remove_item (int Item_id)  {
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
      Pen_width_list[Item_index - 1] = Pen_width_list[Item_index];

   Num_items--;
   assert (Num_items >= 0);
   }

// *******************************************************************
      void GPen_width_owner_draw::Add_item (void* Data_ptr)  {
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
   assert (Num_items < MAX_PEN_WIDTHS);

   int *Int_ptr = (int*) Data_ptr;
   Pen_width_list[Num_items - 1] = *Int_ptr;
   }

// *******************************************************************
      void GPen_width_owner_draw::DrawItem(DRAWITEMSTRUCT far &Draw_info)  {
// *******************************************************************

//  Short description:
//    Draw item

//  Notes:

//  Changes:
//      DPH 8/8/95

//  Calls:

//  Internal variables
      TPen *Pen;                       // Pen to use
      TBrush Brush(TColor::White);

// -------------------- Executable code section ----------------------

   // Create device context object for screen

   TDC Screen_dc(Draw_info.hDC);

   // If this item is selected then draw a box around it and store value in
   // return pointer.

   if ((Draw_info.itemState & ODS_SELECTED) == ODS_SELECTED)
      Pen = new TPen(TColor::Black, 2);

   else
      Pen = new TPen(TColor::White, 2);

   Screen_dc.SelectObject(Brush);
   Screen_dc.SelectObject(*Pen);
   Screen_dc.Rectangle(Draw_info.rcItem);
   delete Pen;

   // Draw our line from left to right.

   Pen = new TPen(TColor::Black, Pen_width_list[Draw_info.itemID]);
   Screen_dc.SelectObject(*Pen);

   int y = (Draw_info.rcItem.top + Draw_info.rcItem.bottom) / 2;
   Screen_dc.MoveTo(Draw_info.rcItem.left, y);
   Screen_dc.LineTo(Draw_info.rcItem.right, y);

   // Delete our GDI objects.

   delete Pen;
   }

// *******************************************************************
      void GPen_width_owner_draw::Set_selection(int Pen_width)  {
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
      Found = (Pen_width_list[Index] == Pen_width);
   Index--;

   if (Found)
      Selected_item = Index;
   else
      assert (FALSE);

   }

