/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         chartcon.cpp
    AUTHOR:


    OVERVIEW
    ========
    Source file for implementation of Chart_control (TControl).
*/

#include <graph\global.h>
#pragma hdrstop

#include <graph\dchart.h>
#include <cl\chartcon.h>


//{{Chart_control Implementation}}

// ------------------------------------------------------------------
//  Short description:
//    constructor                         

//  Notes:

//  Changes:
//    DPH 30/6/1996

// ------------------------------------------------------------------
Chart_control::Chart_control (TWindow*   parent,
                              int        id,
                              TModule*   module):
    TControl(parent, id, module), Chart_list(3, 0, 3)
   {
   // INSERT>> Your constructor code here.

   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 30/6/1996

// ------------------------------------------------------------------

Chart_control::~Chart_control ()
   {
   Destroy();

   // INSERT>> Your destructor code here.

   }

// ------------------------------------------------------------------
//  Short description:
//    Draw chart

//  Notes:

//  Changes:
//    DPH 30/6/1996

// ------------------------------------------------------------------
void Chart_control::DrawItem (DRAWITEMSTRUCT far& drawInfo)
   {
   TControl::DrawItem(drawInfo);

   // INSERT>> Your code here.

   // get a GDC from a device context.

   GDC DC(drawInfo.hDC);

   // is this a print job

   if (drawInfo.rcItem.right < 1000)
      {
      // no - set background brush

      SelectObject(DC, GetStockObject(LTGRAY_BRUSH));
      }

   // draw filled rectangle around control

   ((TDC*) &DC)->Rectangle(drawInfo.rcItem);

   // loop through all charts.

   for (int Chart_index = 0;
        Chart_index < Chart_list.GetItemsInContainer();
        Chart_index++)
      {
      // paint chart.

      Chart_list[Chart_index]->Paint(DC, TRect(drawInfo.rcItem));
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    this routine is only ever called from printout object to send
//    charts to printer.

//  Notes:

//  Changes:
//    DPH 30/6/1996

// ------------------------------------------------------------------
void Chart_control::Paint (TDC& dc, bool erase, TRect& rect)
   {
   TControl::Paint(dc, erase, rect);

   // INSERT>> Your code here.

   // create a DRAWITEMSTRUCT to pass to DrawItem method

   DRAWITEMSTRUCT d;
   d.hDC = dc;
   d.rcItem = rect;

   // loop through all charts

   for (int Chart_index = 0;
        Chart_index < Chart_list.GetItemsInContainer();
        Chart_index++)
      {
      // set the dirty flag so that the chart will set itself up again.

      Chart_list[Chart_index]->Set_dirty();
      }

   // go send charts to printer.

   DrawItem(d);

   // loop through all charts

   for (int Chart_index = 0;
        Chart_index < Chart_list.GetItemsInContainer();
        Chart_index++)
      {
      // set the dirty flag so that the chart will set itself up again.

      Chart_list[Chart_index]->Set_dirty();
      }
   }

