#include <graph\global.h>
#pragma hdrstop

#include <consts.h>
#include <graph\brshstys.h>
#include <assert.h>

// *******************************************************************
      GBrush_styles::GBrush_styles(void)  {
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

   Set_brush_styles(All_brush_styles_list, MAX_BRUSH_STYLES);
   }

// *******************************************************************
      void* GBrush_styles::Get_item (int Item_id)  {
// *******************************************************************

//  Short description:
//    return pointer to item to caller.

//  Notes:

//  Changes:
//      DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   return &Brush_style_list[Item_id];
   }

// *******************************************************************
      void GBrush_styles::Remove_item (int Item_id)  {
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
      Brush_style_list[Item_index - 1] = Brush_style_list[Item_index];

   Num_items--;
   assert (Num_items >= 0);
   }

// *******************************************************************
      void GBrush_styles::Add_item (void* Data_ptr)  {
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
   assert (Num_items < MAX_BRUSH_STYLES);

   Brush_style_enum *Int_ptr = (Brush_style_enum*) Data_ptr;
   Brush_style_list[Num_items - 1] = *Int_ptr;
   }

// *******************************************************************
      void GBrush_styles::DrawItem(DRAWITEMSTRUCT far &Draw_info)  {
// *******************************************************************

//  Short description:
//    Draw item

//  Notes:

//  Changes:
//      DPH 8/8/95

//  Calls:

//  Internal variables
      TPen *Pen;                       // Pen to use
      TBrush *Brush;                   // Brush to use

// -------------------- Executable code section ----------------------

   if (Draw_info.itemID != 0xffff)
      {
      // Create device context object for screen

      GDC Screen_dc(Draw_info.hDC);

      // If this item is selected then draw a box around it and store value in
      // return pointer.

      int Item_id = Draw_info.itemID;

      if ((Draw_info.itemState & ODS_SELECTED) == ODS_SELECTED)
         Pen = new TPen(TColor::Black, 2);

      else
         Pen = new TPen(TColor::White, 2);

      Brush = new TBrush(Brush_style_list[Item_id]);
      Screen_dc.SelectObject(*Brush);
      Screen_dc.SelectObject(*Pen);
      ((TDC*) &Screen_dc)->Rectangle(Draw_info.rcItem);
      delete Pen;
      delete Brush;

      // Fill rectangle with our brush.

      Pen = new TPen(TColor::Gray, 1, Item_id);
      Screen_dc.SelectObject(*Pen);
      Screen_dc.SetTextAlign(TA_LEFT | TA_TOP);

      if (Brush_style_list[Draw_info.itemID] == BS_Solid)
         ((TDC*) &Screen_dc)->TextOut((Draw_info.rcItem.left + Draw_info.rcItem.right)/2,
                            Draw_info.rcItem.top, "Solid");

      else if (Brush_style_list[Draw_info.itemID] == BS_Null)
         ((TDC*) &Screen_dc)->TextOut((Draw_info.rcItem.left + Draw_info.rcItem.right)/2,
                            Draw_info.rcItem.top, "None");

      else
         {
         GBrush Brush;
         Brush.Set_style(Brush_style_list[Draw_info.itemID]);
         Brush.Set_colour(GColour(TColor::Gray));

         Screen_dc.Select_object(Brush);
         ((TDC*) &Screen_dc)->Rectangle(Draw_info.rcItem);
         }

      // Delete our GDI objects.

      delete Pen;
      }
   }

// *******************************************************************
      void GBrush_styles::Get_brush_styles(Brush_style_enum Brush_styles[],
                                           int& Num_brush_styles)  {
// *******************************************************************

//  Short description:
//    get the brush style list.

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
      Brush_styles[Index] = Brush_style_list[Index];

   Num_brush_styles = Num_items;
   }

// *******************************************************************
      void GBrush_styles::Set_brush_styles(Brush_style_enum Brush_styles[],
                                           int Num_brush_styles)  {
// *******************************************************************

//  Short description:
//    set the brush style list.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   for (int Index = 0;
        Index < Num_brush_styles;
        Index++)
      Brush_style_list[Index] = Brush_styles[Index];

   Num_items = Num_brush_styles;
   }

// *******************************************************************
      void GBrush_styles::Set_brush_styles_invert(Brush_style_enum Brush_styles[],
                                                  int Num_brush_styles)  {
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
        Index1 < MAX_BRUSH_STYLES;
        Index1++)
      {
      // loop through the pen styles list passed in.

      bool Found = FALSE;
      for (int Index2 = 0;
           Index2 < Num_brush_styles && !Found;
           Index2++)
         {
         Found = (All_brush_styles_list[Index1] == Brush_styles[Index2]);
         }


      if (!Found)
         {
         Num_items++;
         Brush_style_list[Num_items - 1] = All_brush_styles_list[Index1];
         }
      }
   }

// *******************************************************************
      void GBrush_styles::Set_selection(Brush_style_enum& Brush_style)  {
// *******************************************************************

//  Short description:
//    set the brush style selection.

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
      Found = (Brush_style_list[Index] == Brush_style);
   Index--;

   if (Found)
      Selected_item = Index;
   else
      assert (FALSE);

   }
             
