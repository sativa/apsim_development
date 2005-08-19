#if !defined(__chartcon_h)              // Sentry, use file only if it's not already included.
#define __chartcon_h

/*  Project chartcon
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         chartcon.h
    AUTHOR:


    OVERVIEW
    ========
    Class definition for Chart_control (TControl).
*/

#include <owl\control.h>
#include <graph\global.h>
#include <graph\dchart.h>

//{{TControl = Chart_control}}
// ------------------------------------------------------------------
//  Short description:
//    Object for encapsulating an owner drawn chart control that
//    can be embedded into a dialog or window.

//  Notes:
//    This object will delete all objects added to chart_list automatically
//    in destructor or when Flush is called.

//  Changes:
//    DPH 30/6/1996

// ------------------------------------------------------------------
class huge Chart_control : public TControl
   {
   protected:
      Chart_array  Chart_list;         // pointer to chart list

   public:
      Chart_control (TWindow*   parent,
                     int        Resource_id,
                     TModule*   module = 0);
                                       // constructor.
      virtual ~Chart_control ();       // destructor.

      void Add_chart(DChart* Chart_ptr)
         {
         Chart_list.Add(Chart_ptr);
         }                             // add a chart to the list.
      DChart* Get_chart (int Chart_number)
         {
         return Chart_list[Chart_number];
         }                             // return a chart pointer to caller
      int Get_num_charts (void)
         {
         return Chart_list.GetItemsInContainer();
         }                             // return number of charts to caller
      void Flush(void)
         {
         Chart_list.Flush();
         }                             // empty chart list

      //{{Chart_controlVIRTUAL_BEGIN}}
      public:
          virtual void DrawItem (DRAWITEMSTRUCT far& drawInfo);
          virtual void Paint (TDC& dc, bool erase, TRect& rect);
      //{{Chart_controlVIRTUAL_END}}

   };    //{{Chart_control}}

#endif                                      // __chartcon_h sentry.

