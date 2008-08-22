#if !defined(__texttble_h)              // Sentry, use file only if it's not already included.
#define __texttble_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         texttble.h
    AUTHOR:


    OVERVIEW
    ========
    Class definition for Text_table (TControl).

      These classes are used to send a text table to a DC.
      They work similiarly to a DOS text screen in that they
      use rows and columns.  The world coordinates used are
      rows (y) and columns(x).  All rows and columns numbering start
      at 1.
*/

#include <owl\owlpch.h>
#pragma hdrstop


#include <owl\dc.h>
#include <gobjs\gstring.h>
#include <owl\printer.h>

#define MAX_ITEMS  100;

class Text_table;                      // forward

class Table_item
	{
   protected:
		int x1, y1;								// x/y coordinate of start of object.
	public:
		Table_item(int x, int y)
         {
         x1 = x;
         y1 = y;
         };                      		// constructor
      virtual void Paint(TDC& , Text_table& )  {};
	};

class Table_text_item : public Table_item
	{
	protected:
		GString Text;     					// Text to display.
	public:
		Table_text_item(int x1, int y1, GString& Text);
                                       // constructor
      void Paint(TDC& DC, Text_table& Table_obj);
	};

class Table_rect_item : public Table_item
	{
	protected:
		int x2, y2;                      // last coordinates of item.
      int Stock_brush;                 // stock brush to use.
   public:
      Table_rect_item(int x1, int y1, int x2, int y2);
                                       // constructor.
      void Paint(TDC& DC, Text_table& Table_obj);
      void Set_stock_brush(int Brush = LTGRAY_BRUSH)
         {
         Stock_brush = Brush;
         };                            // set the stock brush to use.
	};

class Table_line_item : public Table_item
	{
	protected:
		int x2, y2;                      // last coordinates of item.
   public:
      Table_line_item(int x1, int y1, int x2, int y2);
                                       // constructor.
      void Paint(TDC& DC, Text_table& Table_obj);
	};

class Text_table  {
protected:
	Table_item * *Table_items;	         // list of all table items.
   int Num_items;                      // Number of items in list.
   int Max_items;                      // maximum number of items allowed.
   TRect Paint_rect;                   // rectangle to paint in.

public:
    Text_table (void);
    virtual ~Text_table ();
    void Add_item(Table_item *Table_item_ptr);
                                       // add item to list of all items.
    int World_to_pixel_x(TDC& DC, int World_x);
                                       // return the pixel coordinate for the
                                       // specified world coordinate
    int World_to_pixel_y(TDC& DC, int World_y);
                                       // return the pixel coordinate for the
                                       // specified world coordinate

//{{Text_tableVIRTUAL_BEGIN}}
public:
    virtual void Paint (TDC& dc, bool erase, TRect& rect);
//{{Text_tableVIRTUAL_END}}
};    //{{Text_table}}

// *******************************************************************
       class Text_table_printout : public TPrintout  {
// *******************************************************************

//  Short description:
//    Printout object for table

//  Notes:

//  Changes:
//    DPH 9/3/95

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      Text_table *Text_table_ptr;      // pointer to table object

//   Protected member functions
//      none

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
		Text_table_printout(const char *title, Text_table *Table_p);
                                       // constructor
		void PrintPage(int , TRect& , unsigned );
                                 		// print the page on the printer
	};

#endif                                      // __table_h sentry.

