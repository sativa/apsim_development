#if !defined(__chartwin_h)              // Sentry, use file only if it's not already included.
#define __chartwin_h

#include <graph\dchart.h>

// *******************************************************************
       class huge Chart_window : public TWindow  {
// *******************************************************************

//  Short description:
//      A chart window that is capable of displaying a chart.

//  Notes:

//  Changes:
//      DPH 24/8/95

// ------------------------- Private section -------------------------

   protected :

//   Protected data
      bool Interactive;                // is this window interactive?

//   Protected member functions
      void Paint (TDC& dc, bool erase, TRect& rect);
                                       // paint the chart in the window.
      void EvLButtonDown (UINT modKeys, TPoint& point);
                                       // the left button was clicked.  Pass
                                       // click to chart object.
      void EvLButtonUp (UINT modKeys, TPoint& point);
                                       // the left button was released.  Pass
                                       // click to chart object.
      void EvMouseMove (UINT modKeys, TPoint& point);
                                       // the mouse was moved.  Pass
                                       // event to chart object.
      void EvRButtonDown (UINT modKeys, TPoint& point);
                                       // the right button was clicked.  Pass
                                       // click to chart object.
      LRESULT EvCommand (UINT id, HWND hWndCtl, UINT notifyCode);
                                       // the user has selected a menu option.
                                       // Pass it to chart object.
      void EvSize(uint , TSize& );     // window has been resized.
      void EvKeyDown(uint key, uint, uint);
                                       // a key was pressed
      void EvKillFocus(HWND);          // window has lost focus


   public :

//   Public data
      DChart Chart;                    // Chart object

//   Public member functions
      Chart_window(TWindow *Parent,
                   const char far *Title,
                   bool Interactive = TRUE,
                   TModule *Module = 0);
                                       // constructor

      DECLARE_RESPONSE_TABLE(Chart_window);
      DECLARE_STREAMABLE( , Chart_window, 1);
   };


#endif