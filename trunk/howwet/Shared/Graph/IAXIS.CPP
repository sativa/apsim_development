#include <graph\global.h>
#pragma hdrstop
#include <graph\daxis.h>
#include <owl\dialog.h>
#include <owl\checkbox.h>
#include <owl\edit.h>
#include <owl\combobox.h>
#include <owl\scrollba.h>
#include <graph\igraph.rh>

// *******************************************************************
       class Axis_properties_dialog : public TDialog {
// *******************************************************************

//  Short description:
//      Axis properties dialog class.

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
      int *Title_distance_ptr;         // pointer to title distance
      int *Tick_size_ptr;              // pointer to tick size
      DAxis::Tick_style_enum *Tick_style_ptr;
                                       // pointer to tick style.
      TScrollBar *Title_distance_scroll;
                                       // pointer to title distance scroll bar
      TScrollBar *Tick_size_scroll;    // pointer to tick size scroll bar
      TComboBox *Tick_style_combo;     // pointer to tick style combo box
      TStatic *Title_distance_static;  // Title distance static control
      TStatic *Tick_size_static;       // Tick size static control

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
      Axis_properties_dialog(TWindow* Parent, int Resource_id, TModule *Module,
                             int *Title_dist, int *Tick_size,
                             DAxis::Tick_style_enum *Tick_style);
                                       // constructor

      DECLARE_RESPONSE_TABLE(Axis_properties_dialog);
   };

DEFINE_RESPONSE_TABLE1(Axis_properties_dialog, TDialog)
   EV_WM_VSCROLL,
END_RESPONSE_TABLE;

// *******************************************************************
       Axis_properties_dialog::Axis_properties_dialog
          (TWindow* Parent, int Resource_id, TModule *Module,
           int *Title_distance, int *Tick_size,
           DAxis::Tick_style_enum *Tick_style)
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

   Title_distance_ptr = Title_distance;
   Tick_size_ptr = Tick_size;
   Tick_style_ptr = Tick_style;

   Title_distance_scroll = new TScrollBar(this, IDC_VSCROLL1);
   Tick_size_scroll = new TScrollBar(this, IDC_VSCROLL2);
   Tick_style_combo = new TComboBox(this, IDC_COMBOBOX1);

   Title_distance_static = new TStatic(this, IDC_STATIC1);
   Tick_size_static = new TStatic(this, IDC_STATIC2);
   }

// *******************************************************************
       void Axis_properties_dialog::SetupWindow(void)  {
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

   Title_distance_scroll->SetRange(0, 50);
   Title_distance_scroll->SetPosition(*Title_distance_ptr);
   EvVScroll(0, 0, *Title_distance_scroll);

   Tick_size_scroll->SetRange(0, 20);
   Tick_size_scroll->SetPosition(*Tick_size_ptr);
   EvVScroll(0, 0, *Tick_size_scroll);

   Tick_style_combo->AddString("in");
   Tick_style_combo->AddString("out");
   Tick_style_combo->AddString("cross");
   if (*Tick_style_ptr == DAxis::in)
      Tick_style_combo->SetSelIndex(0);
   else if (*Tick_style_ptr == DAxis::out)
      Tick_style_combo->SetSelIndex(1);
   else
      Tick_style_combo->SetSelIndex(2);

   }

// *******************************************************************
       bool Axis_properties_dialog::CanClose(void)  {
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

   *Title_distance_ptr = Title_distance_scroll->GetPosition();
   *Tick_size_ptr = Tick_size_scroll->GetPosition();

   if (Tick_style_combo->GetSelIndex() == 0)
      *Tick_style_ptr = DAxis::in;

   else if (Tick_style_combo->GetSelIndex() == 1)
      *Tick_style_ptr = DAxis::out;

   else
      *Tick_style_ptr = DAxis::cross;

   return TRUE;
   }

// *******************************************************************
       void Axis_properties_dialog::EvVScroll
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
   
   if (Window_control == *Title_distance_scroll)
      {
      wsprintf(Text, "%i mm", Title_distance_scroll->GetPosition());
      Title_distance_static->SetText(Text);
      }

   else
      {
      wsprintf(Text, "%i mm", Tick_size_scroll->GetPosition());
      Tick_size_static->SetText(Text);
      }
   }

// *******************************************************************
       class Scale_properties_dialog : public TDialog {
// *******************************************************************

//  Short description:
//      Scale properties dialog class.

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
      float *Minimum_ptr;              // Pointer to storage for minimum
      float *Maximum_ptr;              // Pointer to storage for maximum
      float *Interval_ptr;             // Pointer to storage for interval
      bool *Minimum_fixed_ptr;         // Pointer to storage for minimum fixed
      bool *Maximum_fixed_ptr;         // Pointer to storage for maximum fixed
      bool *Interval_fixed_ptr;        // Pointer to storage for interval fixed
      int *Dec_places_ptr;             // Pointer to storage for dec. places.
      TEdit *Min_edit;                 // Pointer to minimum edit box.
      TEdit *Max_edit;                 // Pointer to maximum edit box.
      TEdit *Int_edit;                 // Pointer to interval edit box.
      TCheckBox *Min_check;            // Pointer to minimum check box
      TCheckBox *Max_check;            // Pointer to maximum check box
      TCheckBox *Int_check;            // Pointer to interval check box.
      TComboBox *Dec_places;           // Pointer to decimal places combo box.

//   Protected member functions
       void SetupWindow(void);         // setup window
       bool CanClose(void);            // Window is about to close
       void Edit1_changed(void);       // Edit box 1 was changed.
       void Edit2_changed(void);       // Edit box 2 was changed.
       void Edit3_changed(void);       // Edit box 3 was changed.


// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      Scale_properties_dialog(TWindow* Parent, int Resource_id, TModule *Module,
                              float *Min_ptr, float *Max_ptr, float *Int_ptr,
                              bool *Min_fixed_ptr, bool *Max_fixed_ptr, bool *Int_fixed_ptr,
                              int *Dec_places_ptr);
                                       // constructor

      DECLARE_RESPONSE_TABLE(Scale_properties_dialog);
   };

DEFINE_RESPONSE_TABLE1(Scale_properties_dialog, TDialog)
   EV_EN_CHANGE(IDC_EDIT1, Edit1_changed),
   EV_EN_CHANGE(IDC_EDIT2, Edit2_changed),
   EV_EN_CHANGE(IDC_EDIT3, Edit3_changed),
END_RESPONSE_TABLE;

// *******************************************************************
       Scale_properties_dialog::Scale_properties_dialog
          (TWindow* Parent, int Resource_id, TModule *Module,
           float *Min_ptr, float *Max_ptr, float *Int_ptr,
           bool *Min_fixed_ptr, bool *Max_fixed_ptr, bool *Int_fixed_ptr,
           int *Dec_places_p)
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

   Min_edit = new TEdit(this, IDC_EDIT1, 20);
   Max_edit = new TEdit(this, IDC_EDIT2, 20);
   Int_edit = new TEdit(this ,IDC_EDIT3, 20);
   Min_check = new TCheckBox(this, IDC_CHECKBOX1);
   Max_check = new TCheckBox(this, IDC_CHECKBOX2);
   Int_check = new TCheckBox(this, IDC_CHECKBOX3);
   Dec_places = new TComboBox(this, IDC_COMBOBOX1);

   Minimum_ptr = Min_ptr;
   Maximum_ptr = Max_ptr;
   Interval_ptr = Int_ptr;
   Minimum_fixed_ptr = Min_fixed_ptr;
   Maximum_fixed_ptr = Max_fixed_ptr;
   Interval_fixed_ptr = Int_fixed_ptr;
   Dec_places_ptr = Dec_places_p;
   }

// *******************************************************************
       void Scale_properties_dialog::SetupWindow(void)  {
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

   Text.Set_real(*Minimum_ptr);
   Min_edit->SetText(Text.c_str());

   Text.Set_real(*Maximum_ptr);
   Max_edit->SetText(Text.c_str());

   Text.Set_real(*Interval_ptr);
   Int_edit->SetText(Text.c_str());

   if (*Minimum_fixed_ptr)
      Min_check->SetCheck(BF_UNCHECKED);
   else
      Min_check->SetCheck(BF_CHECKED);

   if (*Maximum_fixed_ptr)
      Max_check->SetCheck(BF_UNCHECKED);
   else
      Max_check->SetCheck(BF_CHECKED);

   if (*Interval_fixed_ptr)
      Int_check->SetCheck(BF_UNCHECKED);
   else
      Int_check->SetCheck(BF_CHECKED);

   // Fill dec. places combo box.

   Dec_places->AddString("Auto");
   Dec_places->AddString("0");
   Dec_places->AddString("1");
   Dec_places->AddString("2");
   Dec_places->AddString("3");
   Dec_places->AddString("4");
   Dec_places->SetSelIndex(0);
   }

// *******************************************************************
       bool Scale_properties_dialog::CanClose(void)  {
// *******************************************************************

//  Short description:
//    Window about to close.  Save dialog box settings to pointers.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      char Text[50];                   // text from edit box control.

// -------------------- Executable code section ----------------------

   TDialog::CanClose();

   Min_edit->GetText(Text, 50);
   *Minimum_ptr = atof(Text);

   Max_edit->GetText(Text, 50);
   *Maximum_ptr = atof(Text);

   Int_edit->GetText(Text, 50);
   *Interval_ptr = atof(Text);

   *Minimum_fixed_ptr =  (Min_check->GetCheck() != BF_CHECKED);
   *Maximum_fixed_ptr =  (Max_check->GetCheck() != BF_CHECKED);
   *Interval_fixed_ptr = (Int_check->GetCheck() != BF_CHECKED);

   *Dec_places_ptr = Dec_places->GetSelIndex() - 1;
   return TRUE;
   }

// *******************************************************************
       void Scale_properties_dialog::Edit1_changed(void)  {
// *******************************************************************

//  Short description:
//    edit box was changed.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Min_check->SetCheck(BF_UNCHECKED);
   }

// *******************************************************************
       void Scale_properties_dialog::Edit2_changed(void)  {
// *******************************************************************

//  Short description:
//    edit box was changed.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Max_check->SetCheck(BF_UNCHECKED);
   }

// *******************************************************************
       void Scale_properties_dialog::Edit3_changed(void)  {
// *******************************************************************

//  Short description:
//    edit box was changed.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Int_check->SetCheck(BF_UNCHECKED);
   }

// *******************************************************************
       class Label_scale_properties_dialog : public TDialog {
// *******************************************************************

//  Short description:
//      Label Scale properties dialog class.

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
      int *Label_interval_ptr;         // Pointer to storage for label interval
      TEdit *Label_interval;           // Pointer to label interval edit box.

//   Protected member functions
       void SetupWindow(void);         // setup window
       bool CanClose(void);            // Window is about to close


// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      Label_scale_properties_dialog(TWindow* Parent, int Resource_id, TModule *Module,
                              int *Label_interval_p);
                                       // constructor

   };

// *******************************************************************
       Label_scale_properties_dialog::Label_scale_properties_dialog
          (TWindow* Parent, int Resource_id, TModule *Module,
           int *Label_interval_p)
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

   Label_interval = new TEdit(this, IDC_EDIT1, 20);

   Label_interval_ptr = Label_interval_p;
   }

// *******************************************************************
       void Label_scale_properties_dialog::SetupWindow(void)  {
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

   Text.Set_integer(*Label_interval_ptr);
   Label_interval->SetText(Text.c_str());
   }

// *******************************************************************
       bool Label_scale_properties_dialog::CanClose(void)  {
// *******************************************************************

//  Short description:
//    Window about to close.  Save dialog box settings to pointers.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      char Text[50];                   // text from edit box control.

// -------------------- Executable code section ----------------------

   TDialog::CanClose();

   Label_interval->GetText(Text, 50);
   *Label_interval_ptr = atoi(Text);

   return TRUE;
   }
#ifdef INTERACTIVE

// *******************************************************************
      void GMin_max_scale::Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id)  {
// *******************************************************************

//  Short description:
//    Create the popup menu for this object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Popup_menu.AppendMenu(MF_ENABLED, Menu_id, "&Axis scale");
   Menu_id++;
   }

// *******************************************************************
      bool GMin_max_scale::Do_menu_item
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

   ID_is_ours = (Menu_id == User_selected_id);
   if (ID_is_ours)
      Properties(Parent);

   Menu_id++;
   return ID_is_ours;
   }

// *******************************************************************
      void GMin_max_scale::Properties(TWindow& Parent)  {
// *******************************************************************

//  Short description:
//    Display properties dialog box.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      float Min;                       // Minimum axis scale
      float Max;                       // Maximum axis scale
      float Int;                       // Interval on axis.
      bool Min_fixed;                  // Fixed Minimum axis scale ?
      bool Max_fixed;                  // Fixed Maximum axis scale ?
      bool Int_fixed;                  // Fixed Interval on axis ?
      int Dec_places;                  // Decimal places for labels.

// -------------------- Executable code section ----------------------

   Get_min_max_int(Min, Max, Int);
   Min_fixed = Minimum_fixed;
   Max_fixed = Maximum_fixed;
   Int_fixed = Interval_fixed;
   Dec_places = Label_dec_places;

   TModule igraph(DLL_name);
   if (Scale_properties_dialog(&Parent, SCALE_PROP_ID, &igraph,
                               &Min, &Max, &Int,
                               &Min_fixed, &Max_fixed, &Int_fixed,
                               &Dec_places)
                             .Execute() == IDOK)
      {
      Set_minimum(Min, Min_fixed);
      Set_maximum(Max, Max_fixed);
      Set_interval(Int, Int_fixed);
      Set_dec_places(Dec_places);
      Parent.Invalidate();
      }
   }

// *******************************************************************
      void GLabel_scale::Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id)  {
// *******************************************************************

//  Short description:
//    Create the popup menu for this object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Popup_menu.AppendMenu(MF_ENABLED, Menu_id, "&Axis scale");
   Menu_id++;
   }

// *******************************************************************
      bool GLabel_scale::Do_menu_item
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

   ID_is_ours = (Menu_id == User_selected_id);
   if (ID_is_ours)
      Properties(Parent);

   Menu_id++;
   return ID_is_ours;
   }

// *******************************************************************
      void GLabel_scale::Properties(TWindow& Parent)  {
// *******************************************************************

//  Short description:
//    Display properties dialog box.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      int Label_int;                   // Label interval

// -------------------- Executable code section ----------------------

   Label_int = Get_label_interval();

   TModule igraph(DLL_name);
   if (Label_scale_properties_dialog(&Parent, LABEL_SCALE_ID, &igraph,
                               &Label_int)
                             .Execute() == IDOK)
      {
      Set_label_interval(Label_int);
      Parent.Invalidate();
      }
   }

// *******************************************************************
       void DAxis::Draw_selection(GDC& DC)  {
// *******************************************************************

//  Short description:
//    This object is selected.  Draw handles around object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   DC.Set_coord(Coord);
   Draw_handle(DC, Start_point);
   Draw_handle(DC, End_point);

   Title.Draw_selection(DC);
   }

// *******************************************************************
       bool DAxis::Hit_test(GDC& DC, TPoint& Point)  {
// *******************************************************************

//  Short description:
//    Return true if point lies on this line.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      bool Hit = FALSE;                // do we have a hit?
      int x1,y1,x2,y2;                 // pixel coordinates of start and end of line
      static const int Margin = 10;    // Allow 10 pixels either side of line for point.

// -------------------- Executable code section ----------------------

   DC.Set_coord(Coord);

   Hit = Title.Hit_test(DC, Point);
   if (!Hit)
      {
      Start_point.Get_pixel(DC, x1, y1);
      End_point.Get_pixel(DC, x2, y2);

      float Denom = x2 - x1;
      if (Denom == 0)
         {
         // Vertical line.

         if (y1 < y2)
            Hit = (Point.x >= x1 - Margin && Point.x <= x1 + Margin &&
                   Point.y >= y1 - Margin && Point.y <= y2 + Margin);
         else
            Hit = (Point.x >= x1 - Margin && Point.x <= x1 + Margin &&
                   Point.y >= y2 - Margin && Point.y <= y1 + Margin);

         }

      else if (Point.x >= x1 && Point.x <= x2)
         {
         // Ok - we're in the ball park.

         float Slope = (y2 - y1) / Denom;

         int y = y1 + (Point.x - x1) * Slope;
         Hit = (Point.y >= y - Margin && Point.y <= y + Margin);
         }

      else
         Hit = FALSE;
      }

   return Hit;
   }

// *******************************************************************
      void DAxis::Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id)  {
// *******************************************************************

//  Short description:
//    Create the popup menu for this object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   if (Has_axis_scale())
      Axis_scale().Create_popup_menu(Popup_menu, Menu_id);

   Popup_menu.AppendMenu(MF_ENABLED, Menu_id, "&Axis pen");
   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 1, "&Axis title");
   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 2, "&Tick font");
   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 3, "&Grid line pen");
   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 4, "&Properties");
   Menu_id += 5;
   }

// *******************************************************************
      bool DAxis::Do_menu_item
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

   if (Has_axis_scale() &&
       Axis_scale().Do_menu_item(Parent, Menu_id, User_selected_id))
      return TRUE;

   else
      {
      if (User_selected_id == Menu_id)
         Pen.Properties(Parent);

      else if (User_selected_id == Menu_id + 1)
         Title.Properties(Parent);

      else if (User_selected_id == Menu_id + 2)
         Tick_font.Properties(Parent);

      else if (User_selected_id == Menu_id + 3)
         Grid_line_pen.Properties(Parent);

      else if (User_selected_id == Menu_id + 4)
         Properties(Parent);

      else
         ID_is_ours = FALSE;

      Menu_id += 5;
      return ID_is_ours;
      }
   }

// *******************************************************************
      void DAxis::Properties(TWindow& Parent)  {
// *******************************************************************

//  Short description:
//    Display properties dialog box.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      int Title_dist;                  // Current title distance.
      int Tick_siz;                    // Current tick size.
      DAxis::Tick_style_enum Tick_styl;// Current tick style.

// -------------------- Executable code section ----------------------

   // Get current pen settings.

   Title_dist = Title_distance;
   Tick_siz = Tick_size;
   Tick_styl = Tick_style;

   // Execute dialog box.

   TModule igraph(DLL_name);
   if (Axis_properties_dialog(&Parent, AXIS_PROP_ID, &igraph,
                              &Title_dist, &Tick_siz, &Tick_styl)
                              .Execute() == IDOK)
      {
      // User pressed ok - save changes to pen.

      Title_distance = Title_dist;
      Tick_size = Tick_siz;
      Tick_style = Tick_styl;

      Parent.Invalidate();
      }

   }

#endif
