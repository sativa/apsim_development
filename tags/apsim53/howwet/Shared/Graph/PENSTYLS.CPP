#include <graph\global.h>
#pragma hdrstop
#include <graph\penstyls.h>
#include <assert.h>

// *******************************************************************
      GPen_styles::GPen_styles(void)  {
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

   Set_pen_styles(All_pen_styles_list, MAX_PEN_STYLES);
   }

// *******************************************************************
      void* GPen_styles::Get_item (int Item_id)  {
// *******************************************************************

//  Short description:
//    return pointer to item to caller.

//  Notes:

//  Changes:
//      DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   return &Pen_style_list[Item_id];
   }

// *******************************************************************
      void GPen_styles::Remove_item (int Item_id)  {
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
      Pen_style_list[Item_index - 1] = Pen_style_list[Item_index];

   Num_items--;
   assert (Num_items >= 0);
   }

// *******************************************************************
      void GPen_styles::Add_item (void* Data_ptr)  {
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
   assert (Num_items < MAX_PEN_STYLES);

   Pen_style_enum *Int_ptr = (Pen_style_enum*) Data_ptr;
   Pen_style_list[Num_items - 1] = *Int_ptr;
   }

// *******************************************************************
      void GPen_styles::DrawItem(DRAWITEMSTRUCT far &Draw_info)  {
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

   if (Draw_info.itemID != 0xffff)
      {
      // Create device context object for screen

      GDC Screen_dc(Draw_info.hDC);

      // If this item is selected then draw a box around it and store value in
      // return pointer.

//      int Item_id = Draw_info.itemID;

      if ((Draw_info.itemState & ODS_SELECTED) == ODS_SELECTED)
         Pen = new TPen(TColor::Black, 2);

      else
         Pen = new TPen(TColor::White, 2);

      Screen_dc.SelectObject(Brush);
      Screen_dc.SelectObject(*Pen);
      ((TDC*) &Screen_dc)->Rectangle(Draw_info.rcItem);
      delete Pen;

      // Draw our line from left to right.

      Screen_dc.SetTextAlign(TA_CENTER | TA_TOP);

      if (Pen_style_list[Draw_info.itemID] == PS_Null)
         Screen_dc.TextOut((Draw_info.rcItem.left + Draw_info.rcItem.right)/2,
                            Draw_info.rcItem.top, "None");

      else
         {
         GPen Pen;
         Pen.Set_style(Pen_style_list[Draw_info.itemID]);
         Screen_dc.Select_object(Pen);

         int y = (Draw_info.rcItem.top + Draw_info.rcItem.bottom) / 2;
         ((TDC*) &Screen_dc)->MoveTo(Draw_info.rcItem.left, y);
         ((TDC*) &Screen_dc)->LineTo(Draw_info.rcItem.right, y);
         }
      }
   }

// *******************************************************************
      void GPen_styles::Get_pen_styles(Pen_style_enum Pen_styles[],
                                       int& Num_pen_styles)  {
// *******************************************************************

//  Short description:
//    get the list.

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
      Pen_styles[Index] = Pen_style_list[Index];

   Num_pen_styles = Num_items;
   }

// *******************************************************************
      void GPen_styles::Set_pen_styles(Pen_style_enum Pen_styles[],
                                       int Num_pen_styles)  {
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
        Index < Num_pen_styles;
        Index++)
      Pen_style_list[Index] = Pen_styles[Index];

   Num_items = Num_pen_styles;
   }

// *******************************************************************
      void GPen_styles::Set_pen_styles_invert(Pen_style_enum Pen_styles[],
                                              int Num_pen_styles)  {
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
        Index1 < MAX_PEN_STYLES;
        Index1++)
      {
      // loop through the pen styles list passed in.

      bool Found = FALSE;
      for (int Index2 = 0;
           Index2 < Num_pen_styles && !Found;
           Index2++)
         {
         Found = (All_pen_styles_list[Index1] == Pen_styles[Index2]);
         }


      if (!Found)
         {
         Num_items++;
         Pen_style_list[Num_items - 1] = All_pen_styles_list[Index1];
         }
      }
   }

// *******************************************************************
      void GPen_styles::Set_selection(Pen_style_enum& Pen_style)  {
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
      Found = (Pen_style_list[Index] == Pen_style);
   Index--;

   if (Found)
      Selected_item = Index;
   else
      assert (FALSE);

   }

