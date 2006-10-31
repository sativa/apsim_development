/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         tblecont.cpp
    AUTHOR:


    OVERVIEW
    ========
    Source file for implementation of Text_table
*/

#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include <gobjsx\texttble.h>

Text_table::Text_table (void)
{
    // INSERT>> Your constructor code here.

    Max_items = 50;
    Table_items = new Table_item*[Max_items];
    Num_items = 0;
}


Text_table::~Text_table ()
{

    // INSERT>> Your destructor code here.

    delete [] Table_items;

}

// *******************************************************************
      void Text_table::Add_item (Table_item *Table_item_ptr)  {
// *******************************************************************

//  Short description:
//    Add an item to the item list.

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   if (Num_items < Max_items)
      Table_items[Num_items] = Table_item_ptr;

   Num_items++;
   }

// *******************************************************************
      void Text_table::Paint (TDC& dc, bool , TRect& rect)  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   Paint_rect = rect;

   // loop through each table item and paint it.

   for (int Item = 0;
        Item < Num_items;
        Item++)
      {
      Table_items[Item]->Paint(dc, *this);
      }
   }

// *******************************************************************
      int Text_table::World_to_pixel_x(TDC& DC, int World_x)  {
// *******************************************************************

//  Short description:
//    return the pixel coordinate for the specified world coordinate

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   int Width = DC.GetTextExtent("X", 1).cx;
   return Paint_rect.left + (World_x - 1) * Width;
   }

// *******************************************************************
      int Text_table::World_to_pixel_y(TDC& DC, int World_y)  {
// *******************************************************************

//  Short description:
//    return the pixel coordinate for the specified world coordinate

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   int Height = DC.GetTextExtent("X", 1).cy;
   return Paint_rect.top + (World_y - 1) * Height;
   }

// *******************************************************************
      Table_text_item::Table_text_item(int x1, int y1, GString& St)
         : Table_item(x1, y1)   {
// *******************************************************************

//  Short description:
//    Paint the item on the DC

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Text = St;
   }

// *******************************************************************
      Table_rect_item::Table_rect_item(int x1, int y1, int x, int y)
         : Table_item(x1, y1)   {
// *******************************************************************

//  Short description:
//    Paint the item on the DC

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   x2 = x;
   y2 = y;
   Stock_brush = WHITE_BRUSH;
   }

// *******************************************************************
      Table_line_item::Table_line_item(int x1, int y1, int x, int y)
         : Table_item(x1, y1)   {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   x2 = x;
   y2 = y;
   }

// *******************************************************************
      void Table_text_item::Paint(TDC& DC, Text_table& Table)  {
// *******************************************************************

//  Short description:
//    Paint the item on the DC

//  Notes:

//  Changes:
//    DPH

//  Internal variables
   int Height = DC.GetTextExtent("X", 1).cy;

// -------------------- Executable code section ----------------------

   DC.SetTextAlign(TA_LEFT | TA_BOTTOM);
   DC.SetBkMode(TRANSPARENT);
   DC.TextOut(Table.World_to_pixel_x(DC, x1),
              Table.World_to_pixel_y(DC, y1) + Height / 2,
              Text.c_str());
   }

// *******************************************************************
      void Table_rect_item::Paint(TDC& DC, Text_table& Table)  {
// *******************************************************************

//  Short description:
//    Paint the item on the DC

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   HBRUSH Stock_object = (HBRUSH) GetStockObject(Stock_brush);
   DC.SelectObject(Stock_object);
   DC.Rectangle(Table.World_to_pixel_x(DC, x1),
                Table.World_to_pixel_y(DC, y1),
                Table.World_to_pixel_x(DC, x2),
                Table.World_to_pixel_y(DC, y2));
   }

// *******************************************************************
      void Table_line_item::Paint(TDC& DC, Text_table& Table)  {
// *******************************************************************

//  Short description:
//    Paint the item on the DC

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   DC.MoveTo(Table.World_to_pixel_x(DC, x1),
             Table.World_to_pixel_y(DC, y1));
   DC.LineTo(Table.World_to_pixel_x(DC, x2),
             Table.World_to_pixel_y(DC, y2));
   }

// *******************************************************************
       Text_table_printout::Text_table_printout
          (const char *Title,
           Text_table *Text_table_p)
      	: TPrintout(Title)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 9/3/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

	Text_table_ptr = Text_table_p;
	}

// *******************************************************************
       void Text_table_printout::PrintPage(int , TRect& r, unsigned )  {
// *******************************************************************

//  Short description:
//    Print the calender on the printer

//  Notes:

//  Changes:
//    DPH 9/3/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Text_table_ptr->Paint(*DC, TRUE, r);
	}

