#include <graph\global.h>
#pragma hdrstop

#include <consts.h>
#include <graph\colours.h>
#include <assert.h>

// *******************************************************************
      GColours::GColours(void)  {
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

   Set_colour_styles(All_colours_list, MAX_COLOURS);
   }

// *******************************************************************
      void* GColours::Get_item (int Item_id)  {
// *******************************************************************

//  Short description:
//    return pointer to item to caller.

//  Notes:

//  Changes:
//      DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   return &Colour_list[Item_id];
   }

// *******************************************************************
      void GColours::Remove_item (int Item_id)  {
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
      Colour_list[Item_index - 1] = Colour_list[Item_index];

   Num_items--;
   assert (Num_items >= 0);
   }

// *******************************************************************
      void GColours::Add_item (void* Data_ptr)  {
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
   assert (Num_items < MAX_COLOURS);

   GColour *Colour_ptr = (GColour*) Data_ptr;
   Colour_list[Num_items - 1] = *Colour_ptr;
   }

// *******************************************************************
      void GColours::DrawItem(DRAWITEMSTRUCT far &Draw_info)  {
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

      TDC Screen_dc(Draw_info.hDC);

      // Create and select the pen and brush to use.

      Brush = new TBrush(Colour_list[(int) Draw_info.itemID]);

      if ((Draw_info.itemState & ODS_SELECTED) == ODS_SELECTED)
         Pen = new TPen(TColor::Black, 2);
      else
         Pen = new TPen(TColor::Black, 1, PS_NULL);

      Screen_dc.SelectObject(*Pen);
      Screen_dc.SelectObject(*Brush);

      // Draw our rectangle of colour.

      Screen_dc.Rectangle(Draw_info.rcItem);

      // Delete our GDI objects.

      delete Pen;
      delete Brush;
      }
   }

// *******************************************************************
      void GColours::Get_colour_styles(GColour Colours[],
                                       int& Num_colours)  {
// *******************************************************************

//  Short description:
//    get the colour list.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   for (int Colour_index = 0;
        Colour_index < Num_items;
        Colour_index++)
      Colours[Colour_index] = Colour_list[Colour_index];

   Num_colours = Num_items;
   }

// *******************************************************************
      void GColours::Set_colour_styles(GColour Colours[],
                                       int Num_colours)  {
// *******************************************************************

//  Short description:
//    set the colour list.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   for (int Colour_index = 0;
        Colour_index < Num_colours;
        Colour_index++)
      Colour_list[Colour_index] = Colours[Colour_index];

   Num_items = Num_colours;
   }

// *******************************************************************
      void GColours::Set_colour_styles_invert(GColour Colours[],
                                              int Num_colours)  {
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
        Index1 < MAX_COLOURS;
        Index1++)
      {
      // loop through the pen styles list passed in.

      bool Found = FALSE;
      for (int Index2 = 0;
           Index2 < Num_colours && !Found;
           Index2++)
         {
         Found = (All_colours_list[Index1] == Colours[Index2]);
         }


      if (!Found)
         {
         Num_items++;
         Colour_list[Num_items - 1] = All_colours_list[Index1];
         }
      }
   }

// *******************************************************************
      void GColours::Set_selection(GColour& Colour)  {
// *******************************************************************

//  Short description:
//    set the colour selection.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   bool Found = FALSE;
   int Colour_index;
   for (Colour_index = 0;
        Colour_index < Num_items && !Found;
        Colour_index++)
      Found = (Colour_list[Colour_index] == Colour);
   Colour_index--;

   if (Found)
      Selected_item = Colour_index;
   else
      assert (FALSE);

   }

