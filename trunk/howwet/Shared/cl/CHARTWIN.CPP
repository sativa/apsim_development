#include <graph\global.h>
#pragma hdrstop
#include <cl\chartwin.h>

DEFINE_RESPONSE_TABLE1(Chart_window, TWindow)
    EV_WM_LBUTTONDOWN,
    EV_WM_LBUTTONUP,
    EV_WM_MOUSEMOVE,
    EV_WM_RBUTTONDOWN,
    EV_WM_SIZE,
    EV_WM_KEYDOWN,
    EV_WM_KILLFOCUS,
END_RESPONSE_TABLE;

IMPLEMENT_CASTABLE1(Chart_window, TWindow);
IMPLEMENT_STREAMABLE1(Chart_window, TWindow);

// *******************************************************************
      void Chart_window::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of Chart_window to the passed opstream.

//  Notes:

//  Changes:
//      DPH 28/9/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

//   WriteBaseObject( (TWindow*) GetObject(), os);
   os << GetObject()->Chart;
   }

// *******************************************************************
      void *Chart_window::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of Chart_window from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 28/9/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

//   ReadBaseObject( (TWindow*) GetObject(), is);
   is >> GetObject()->Chart;

   return GetObject();
   }

// *******************************************************************
      Chart_window::Chart_window(TWindow *Parent,
                                 const char far *Title,
                                 bool User_interactive,
                                 TModule *Module)
         : TWindow(Parent, Title, Module)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 16/8/95

//  Calls:

//  Assertions

//  Internal variables

// -------------------- Executable code section ----------------------

   Interactive = User_interactive;
   }

// *******************************************************************
      void Chart_window::Paint (TDC& dc, bool erase, TRect& rect)  {
// *******************************************************************

//  Short description:
//    paint the chart in the window.

//  Notes:

//  Changes:
//    DPH 16/8/95

//  Calls:

//  Assertions

//  Internal variables

// -------------------- Executable code section ----------------------

   TWindow::Paint(dc, erase, rect);

   GDC DC(dc);
   TRect DC_dimensions;
   GetClientRect(DC_dimensions);
   if (rect.right > DC_dimensions.right &&
       rect.bottom > DC_dimensions.bottom)
      DC_dimensions = rect;

   Chart.Paint(DC, DC_dimensions);
   }

// *******************************************************************
      void Chart_window::EvLButtonDown (UINT modKeys, TPoint& point)  {
// *******************************************************************

//  Short description:
//    the left button was clicked.  Pass click to chart object.

//  Notes:

//  Changes:
//    DPH 16/8/95

//  Calls:

//  Assertions

//  Internal variables

// -------------------- Executable code section ----------------------

   TWindow::EvLButtonDown(modKeys, point);

   // INSERT>> Your code here.

   if (Interactive)
      Chart.Left_button_down(*this, point);
   }

// *******************************************************************
      void Chart_window::EvLButtonUp (UINT modKeys, TPoint& point)  {
// *******************************************************************

//  Short description:
//    the left button was released.  Pass click to chart object.

//  Notes:

//  Changes:
//    DPH 16/8/95

//  Calls:

//  Assertions

//  Internal variables

// -------------------- Executable code section ----------------------

   TWindow::EvLButtonUp(modKeys, point);

   // INSERT>> Your code here.

   if (Interactive)
      Chart.Left_button_up(point);
   }

// *******************************************************************
      void Chart_window::EvMouseMove (UINT modKeys, TPoint& point)  {
// *******************************************************************

//  Short description:
//    the mouse was moved.  Pass event to chart object.

//  Notes:

//  Changes:
//    DPH 16/8/95

//  Calls:

//  Assertions

//  Internal variables

// -------------------- Executable code section ----------------------

   TWindow::EvMouseMove(modKeys, point);

   // INSERT>> Your code here.

   if (Interactive)
      Chart.Mouse_move(point);
   }

// *******************************************************************
      void Chart_window::EvRButtonDown (UINT modKeys, TPoint& point)  {
// *******************************************************************

//  Short description:
//    the right button was clicked.  Pass click to chart object.

//  Notes:

//  Changes:
//    DPH 16/8/95

//  Calls:

//  Assertions

//  Internal variables

// -------------------- Executable code section ----------------------

   TWindow::EvRButtonDown(modKeys, point);

   // INSERT>> Your code here.

   if (Interactive)
      Chart.Right_button_down(*this, point);
   }

// *******************************************************************
      LRESULT Chart_window::EvCommand (UINT id, HWND hWndCtl, UINT notifyCode)  {
// *******************************************************************

//  Short description:
//    the user has selected a menu option.  Pass it to chart object.

//  Notes:

//  Changes:
//    DPH 16/8/95

//  Calls:

//  Assertions

//  Internal variables

// -------------------- Executable code section ----------------------

   LRESULT Not_used = TWindow::EvCommand(id, hWndCtl, notifyCode);

   // INSERT>> Your code here.

   if (Interactive)
      Chart.Command(*this, id);

   return Not_used;
   }


// *******************************************************************
      void Chart_window::EvSize(uint , TSize& )  {
// *******************************************************************

//  Short description:
//    window has been resized.

//  Notes:

//  Changes:
//    DPH 16/8/95

//  Calls:

//  Assertions

//  Internal variables

// -------------------- Executable code section ----------------------

   Chart.Set_dirty();
   }

// *******************************************************************
      void Chart_window::EvKeyDown(uint key, uint , uint)  {
// *******************************************************************

//  Short description:
//    a key was pressed.

//  Notes:

//  Changes:
//    DPH 16/8/95

//  Calls:

//  Assertions

//  Internal variables

// -------------------- Executable code section ----------------------

   if (key == VK_DELETE)
      {
      Chart.Objects.Delete_current_object();
      Invalidate();
      }
   }

// *******************************************************************
      void Chart_window::EvKillFocus(HWND Window_handle)  {
// *******************************************************************

//  Short description:
//    window has lost focus.

//  Notes:

//  Changes:
//    DPH 16/8/95

//  Calls:

//  Assertions

//  Internal variables

// -------------------- Executable code section ----------------------

   TWindow::EvKillFocus(Window_handle);
   if (Chart.Objects.Get_select() >= 0)
      {
      Chart.Objects.Select(-1, FALSE);
      Invalidate();
      }
   }

void chart_window_dummy(void)
   {
   new DSeries_array(1);
   new Object_array(1);
   }
