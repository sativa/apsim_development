#include <graph\global.h>
#pragma hdrstop

#include <graph\dobject.h>
#include <graph\dseries.h>
#include <graph\igraph.rh>

#include <owl\dialog.h>
#include <owl\scrollba.h>
#include <owl\static.h>
#include <values.h>
#include <graph\markers.h>

// *******************************************************************
       class Column_properties_dialog : public TDialog {
// *******************************************************************

//  Short description:
//      column properties dialog class.

//  Notes:

//  Changes:
//      DPH 8/8/95

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      int *Bar_width_ptr;              // pointer to bar width
      TScrollBar *Bar_width_scroll;
                                       // pointer to bar width scroll bar
      TStatic *Bar_width_static;       // Bar width static control

//   Protected member functions
       void SetupWindow(void);         // setup window
       bool CanClose(void);            // Window is about to close
       void EvVScroll(UINT Scroll_code, UINT Thumb_pos, HWND Window_control);
                                       // A scroll bar was changed.


// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      Column_properties_dialog(TWindow* Parent, int Resource_id, TModule *Module,
                       int *Bar_wid);
                                       // constructor

      DECLARE_RESPONSE_TABLE(Column_properties_dialog);
   };

DEFINE_RESPONSE_TABLE1(Column_properties_dialog, TDialog)
   EV_WM_VSCROLL,
END_RESPONSE_TABLE;

// *******************************************************************
       Column_properties_dialog::Column_properties_dialog
          (TWindow* Parent, int Resource_id, TModule *Module,
           int *Bar_wid)
          : TDialog(Parent, Resource_id, Module)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Bar_width_ptr = Bar_wid;
   Bar_width_scroll = new TScrollBar(this, IDC_VSCROLL1);
   Bar_width_static = new TStatic(this, IDC_STATIC1);
   }

// *******************************************************************
       void Column_properties_dialog::SetupWindow(void)  {
// *******************************************************************

//  Short description:
//    setup window

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      GString Text;                    // Our text string.

// -------------------- Executable code section ----------------------

   TDialog::SetupWindow();

   Bar_width_scroll->SetRange(0, 50);
   Bar_width_scroll->SetPosition(*Bar_width_ptr);
   EvVScroll(0, 0, *Bar_width_scroll);
   }

// *******************************************************************
       bool Column_properties_dialog::CanClose(void)  {
// *******************************************************************

//  Short description:
//    Window about to close.  Save dialog box settings to pointers.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   TDialog::CanClose();
   *Bar_width_ptr = Bar_width_scroll->GetPosition();

   return TRUE;
   }

// *******************************************************************
       void Column_properties_dialog::EvVScroll
          (UINT Scroll_code, UINT Thumb_pos, HWND Window_control)  {
// *******************************************************************

//  Short description:
//    A scroll bar was changed.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      char Text[50];                   // Text string to display in static

// -------------------- Executable code section ----------------------

   TDialog::EvVScroll(Scroll_code, Thumb_pos, Window_control);

   wsprintf(Text, "%i mm", Bar_width_scroll->GetPosition());
   Bar_width_static->SetText(Text);
   }


// *******************************************************************
       class Marker_properties_dialog : public TDialog {
// *******************************************************************

//  Short description:
//      Marker properties dialog class.

//  Notes:

//  Changes:
//      DPH 8/8/95

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      TScrollBar *Marker_size_scroll;
                                       // pointer to marker size scroll bar
      TStatic *Marker_size_static;     // Marker size static control

//   Protected member functions
       void SetupWindow(void);         // setup window
       bool CanClose(void);            // Window is about to close
       void EvVScroll(UINT Scroll_code, UINT Thumb_pos, HWND Window_control);
                                       // A scroll bar was changed.


// -------------------------- Public section -------------------------

   public :

//   Public data
      int         Marker_size;
      GMarkers    Markers;

//   Public member functions
      Marker_properties_dialog(TWindow* Parent, int Resource_id, TModule *Module);

                                       // constructor

      DECLARE_RESPONSE_TABLE(Marker_properties_dialog);
   };

DEFINE_RESPONSE_TABLE1(Marker_properties_dialog, TDialog)
   EV_WM_VSCROLL,
END_RESPONSE_TABLE;

// *******************************************************************
       Marker_properties_dialog::Marker_properties_dialog
          (TWindow* Parent, int Resource_id, TModule *Module)
          : TDialog(Parent, Resource_id, Module)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Marker_size_scroll = new TScrollBar(this, IDC_VSCROLL1);
   Marker_size_static = new TStatic(this, IDC_STATIC1);

   new GOwner_draw_combo (this,
                          IDC_COMBOBOX1,
                          NULL,
                          &Markers);
   }

// *******************************************************************
       void Marker_properties_dialog::SetupWindow(void)  {
// *******************************************************************

//  Short description:
//    setup window

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      GString Text;                    // Our text string.

// -------------------- Executable code section ----------------------

   TDialog::SetupWindow();

   Marker_size_scroll->SetRange(0, 50);
   Marker_size_scroll->SetPosition(Marker_size);
   EvVScroll(0, 0, *Marker_size_scroll);
   }

// *******************************************************************
       bool Marker_properties_dialog::CanClose(void)  {
// *******************************************************************

//  Short description:
//    Window about to close.  Save dialog box settings to pointers.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   TDialog::CanClose();

   Marker_size = Marker_size_scroll->GetPosition();

   return TRUE;
   }

// *******************************************************************
       void Marker_properties_dialog::EvVScroll
          (UINT Scroll_code, UINT Thumb_pos, HWND Window_control)  {
// *******************************************************************

//  Short description:
//    A scroll bar was changed.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      char Text[50];                   // Text string to display in static

// -------------------- Executable code section ----------------------

   TDialog::EvVScroll(Scroll_code, Thumb_pos, Window_control);

   wsprintf(Text, "%i mm", Marker_size_scroll->GetPosition());
   Marker_size_static->SetText(Text);
   }

#ifdef INTERACTIVE

// *******************************************************************
      void DMarker::Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id)  {
// *******************************************************************

//  Short description:
//    Create the popup menu for this object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 0, "&Marker pen");
   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 1, "&Marker brush");
   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 2, "&Marker properties");
   Popup_menu.AppendMenu(MF_SEPARATOR);
   Menu_id += 3;
   }

// *******************************************************************
      bool DMarker::Do_menu_item
         (TWindow& Parent, int& Menu_id, int User_selected_id)  {
// *******************************************************************

//  Short description:
//    If the user_selected_id belongs to us then return TRUE and perform
//    appropriate actions.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      bool ID_is_ours = TRUE;          // Is Id ours?

// -------------------- Executable code section ----------------------

   if (User_selected_id == Menu_id)
      Pen.Properties(Parent);

   else if (User_selected_id == Menu_id + 1)
      Brush.Properties(Parent);

   else if (User_selected_id == Menu_id + 2)
      Properties(Parent);

   else
      ID_is_ours = FALSE;

   Menu_id += 3;

   return ID_is_ours;
   }

// *******************************************************************
      void DMarker::Properties(TWindow& Parent)  {
// *******************************************************************

//  Short description:
//    Display properties dialog box.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
   Marker_type_enum Mark_type;         // Our marker type
   int Mark_size;                      // Our marker size

// -------------------- Executable code section ----------------------

   Mark_type = Marker_type;
   Mark_size = Marker_size;

   TModule igraph(DLL_name);
   Marker_properties_dialog Marker_dlg(&Parent, MARKER_PROP_ID, &igraph);

   Marker_dlg.Markers.Set_selection(Mark_type);
   Marker_dlg.Marker_size = Mark_size;

   if (Marker_dlg.Execute() == IDOK)
      {
      Set_marker_type(Marker_dlg.Markers.Get_selection());
      Set_marker_size(Marker_dlg.Marker_size);
      Parent.Invalidate();
      }
   }

// *******************************************************************
       bool DMarker::Hit_test(GDC& DC, TPoint& Point)  {
// *******************************************************************

//  Short description:
//    Return true if point lies on this object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   DC.Set_coord(Coord);

//   int Half_marker_size = Marker_size / 2;

   DRectangle Rect;
   Rect.Point1 = XYPoint;
   Rect.Point2 = XYPoint;

   return Rect.Hit_test(DC, Point);
   }
// *******************************************************************
      void DXY_scatter_series::Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id)  {
// *******************************************************************

//  Short description:
//    Create the popup menu for this object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Marker.Create_popup_menu(Popup_menu, Menu_id);
   Popup_menu.AppendMenu(MF_ENABLED, Menu_id, "&Line pen");
   Menu_id++;
   }

// *******************************************************************
      bool DXY_scatter_series::Do_menu_item
         (TWindow& Parent, int& Menu_id, int User_selected_id)  {
// *******************************************************************

//  Short description:
//    If the user_selected_id belongs to us then return TRUE and perform
//    appropriate actions.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      bool ID_is_ours = TRUE;          // Is Id ours?

// -------------------- Executable code section ----------------------

   if (Marker.Do_menu_item(Parent, Menu_id, User_selected_id))
      return TRUE;

   else
      {
      if (User_selected_id == Menu_id)
         Pen.Properties(Parent);

      else
         ID_is_ours = FALSE;

      Menu_id ++;
      return ID_is_ours;
      }
   }

// *******************************************************************
       bool DXY_scatter_series::Hit_test(GDC& DC, TPoint& Point)  {
// *******************************************************************

//  Short description:
//    Return true if point lies on this line.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      bool Hit = FALSE;                // do we have a hit?
      DLine Series_line;               // line between markers.
      float Previous_x = MAXFLOAT;
      float Previous_y = MAXFLOAT;

// -------------------- Executable code section ----------------------

   DC.Set_coord(Coord);

   // Loop through each marker and see if user has clicked on a marker
   // or a line.

   for (int Data_point = 0;
        Data_point < Data[0]->Get_num_elements() && !Hit;
        Data_point++)
      {
      float x = Data[0]->Get_real(Data_point);
      float y = Data[1]->Get_real(Data_point);

      if (x < MISSING_VALUE && y < MISSING_VALUE)
         {
         Marker.XYPoint.Set_logical(x, y, x_axis_link, y_axis_link);
         Hit = Marker.Hit_test(DC, Point);
         if (!Hit && Previous_x < MISSING_VALUE)
            {
            // check the line between this point and the previous one.

            Series_line.Point1.Set_logical(Previous_x, Previous_y, x_axis_link, y_axis_link);
            Series_line.Point2.Set_logical(x, y, x_axis_link, y_axis_link);
            Hit = Series_line.Hit_test(DC, Point);
            }
         Previous_x = x;
         Previous_y = y;
         }
      }

   return Hit;
   }

// *******************************************************************
       void DXY_scatter_series::Draw_selection(GDC& DC)  {
// *******************************************************************

//  Short description:
//    This object is selected.  Draw handles around object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      int Num_points;                  // number of data points.
      GPoint Point;                    // point to draw handle on.
      float x, y;                      // x and y coords
      float Old_x = -MAXFLOAT;         // x and y coords of previous point.
      float Old_y = -MAXFLOAT;

// -------------------- Executable code section ----------------------

   DC.Set_coord(Coord);

   Num_points = Data[0]->Get_num_elements();

   // Draw 10 handles on series points.

   int Handle_interval = Num_points / 10;
   Handle_interval = max(1, Handle_interval);

   for (int Point_number = 0;
        Point_number < Num_points;
        Point_number += Handle_interval)
      {
      x = Data[0]->Get_real(Point_number);
      y = Data[1]->Get_real(Point_number);
      if ((x < MISSING_VALUE && y < MISSING_VALUE) &&
          (x != Old_x || y != Old_y))
         {
         Point.Set_logical(x, y, x_axis_link, y_axis_link);
         Draw_handle(DC, Point);
         Old_x = x;
         Old_y = y;
         }
      }

   // Make sure there is a handle on last point.

   x = Data[0]->Get_real(Num_points - 1);
   y = Data[1]->Get_real(Num_points - 1);

   if ((x < MISSING_VALUE && y < MISSING_VALUE) &&
       (x != Old_x || y != Old_y))
      {
      Point.Set_logical(x, y, x_axis_link, y_axis_link);
      Draw_handle(DC, Point);
      }
   }

// *******************************************************************
      void DColumn_series::Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id)  {
// *******************************************************************

//  Short description:
//    Create the popup menu for this object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 0, "Bar &pen");
   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 1, "Bar &brush");
   Popup_menu.AppendMenu(MF_SEPARATOR);
   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 2, "&Properties");
   Menu_id += 2;
   }

// *******************************************************************
      bool DColumn_series::Do_menu_item
         (TWindow& Parent, int& Menu_id, int User_selected_id)  {
// *******************************************************************

//  Short description:
//    If the user_selected_id belongs to us then return TRUE and perform
//    appropriate actions.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      bool ID_is_ours = TRUE;          // Is Id ours?

// -------------------- Executable code section ----------------------

   if (User_selected_id == Menu_id)
      Pen.Properties(Parent);

   else if (User_selected_id == Menu_id + 1)
      Brush.Properties(Parent);

   else if (User_selected_id == Menu_id + 2)
      Properties(Parent);

   else
      ID_is_ours = FALSE;

   Menu_id += 2;

   return ID_is_ours;
   }

// *******************************************************************
       bool DColumn_series::Hit_test(GDC& DC, TPoint& Point)  {
// *******************************************************************

//  Short description:
//    Return true if point lies on this line.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      bool Hit = FALSE;                // do we have a hit?
      DRectangle Rect;                 // rectangle object used for hittest

// -------------------- Executable code section ----------------------

   DC.Set_coord(Coord);

   // Loop through each marker and see if user has clicked on a marker
   // or a line.

   for (int Data_point = 0;
        Data_point < Data[0]->Get_num_elements() && !Hit;
        Data_point++)
      {
      if (Get_rect(DC, Data_point, Rect))
         Hit = Rect.Hit_test(DC, Point);
      }

   return Hit;
   }

// *******************************************************************
       void DColumn_series::Draw_selection(GDC& DC)  {
// *******************************************************************

//  Short description:
//    This object is selected.  Draw handles around object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      int Num_points;                  // number of data points.
      DRectangle Rect;                 // rectangle object.

// -------------------- Executable code section ----------------------

   DC.Set_coord(Coord);

   Num_points = Data[0]->Get_num_elements();

   // Draw a handle on all columns

   for (int Point_num = 0;
        Point_num < Num_points;
        Point_num++)
      {
      if (Get_rect (DC, Point_num, Rect))
         Rect.Draw_selection (DC);
      }
   }

// *******************************************************************
      void DColumn_series::Properties(TWindow& Parent)  {
// *******************************************************************

//  Short description:
//    Display properties dialog box.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      int Bar_wid;                     // our bar width

// -------------------- Executable code section ----------------------

   Bar_wid = Bar_width;

   TModule igraph(DLL_name);
   if (Column_properties_dialog(&Parent, COLUMN_PROP_ID, &igraph,
                                &Bar_wid)
                               .Execute() == IDOK)
      {
      Set_bar_width(Bar_wid);
      Parent.Invalidate();
      }
   }

#endif

