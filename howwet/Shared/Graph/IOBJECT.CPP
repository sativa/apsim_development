#include <graph\global.h>
#pragma hdrstop

#include <graph\dobject.h>

#include <owl\dialog.h>
#include <owl\edit.h>
#include <owl\checkbox.h>
#include <graph\igraph.rh>

// *******************************************************************
       class Text_properties_dialog : public TDialog {
// *******************************************************************

//  Short description:
//      Pen properties dialog class.

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
      GString *Text_ptr;               // pointer to text string
      GFont *Font_ptr;                 // pointer to font object
      bool *Border_ptr;                // pointer to border boolean.
      TEdit *Title_edit;               // pointer to title edit control
      TCheckBox *Border_check;         // pointer to border check box control

//   Protected member functions
      void Font_button(void);          // font button was pressed.
      void SetupWindow(void);          // setup dialog box.
      bool CanClose(void);             // About to close the dialog box.

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      Text_properties_dialog(TWindow* Parent, int Resource_id, TModule *Module,
                             GString *Text, GFont *Font, bool *Border);
                                       // constructor

      DECLARE_RESPONSE_TABLE(Text_properties_dialog);
   };

DEFINE_RESPONSE_TABLE1(Text_properties_dialog, TDialog)
   EV_BN_CLICKED(IDC_PUSHBUTTON1, Font_button),
END_RESPONSE_TABLE;

// *******************************************************************
       Text_properties_dialog::Text_properties_dialog
          (TWindow* Parent, int Resource_id, TModule *Module,
           GString *Text, GFont *Font, bool *Border)
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

   Title_edit = new TEdit(this, IDC_EDIT1, 200);
   Border_check = new TCheckBox(this, IDC_CHECKBOX1);

   Text_ptr = Text;
   Font_ptr = Font;
   Border_ptr = Border;
   }

// *******************************************************************
       void Text_properties_dialog::SetupWindow(void)  {
// *******************************************************************

//  Short description:
//    setup this window

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   TDialog::SetupWindow();

   Title_edit->SetText(Text_ptr->c_str());

   if (*Border_ptr)
      Border_check->SetCheck(BF_CHECKED);
   else
      Border_check->SetCheck(BF_UNCHECKED);
   }

// *******************************************************************
       bool Text_properties_dialog::CanClose(void)  {
// *******************************************************************

//  Short description:
//    dialog is about to close.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      char Text[200];                  // Text from control box

// -------------------- Executable code section ----------------------

   TDialog::CanClose();

   Title_edit->GetText(Text, 200);
   (*Text_ptr) = Text;

   *Border_ptr = (Border_check->GetCheck() == BF_CHECKED);

   return TRUE;
   }

// *******************************************************************
       void Text_properties_dialog::Font_button(void)  {
// *******************************************************************

//  Short description:
//    font button has been clicked.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Font_ptr->Properties(*this);
   }
#ifdef INTERACTIVE

// *******************************************************************
       bool Rect_hit_test(TPoint Point, int x1, int y1, int x2, int y2)  {
// *******************************************************************

//  Short description:
//    Return true if point x,y lies in rect.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      bool Hit = FALSE;                // do we have a hit?

// -------------------- Executable code section ----------------------

   if (x2 > x1)
      Hit = Point.x >= x1 - HIT_TEST_MARGIN && Point.x <= x2 + HIT_TEST_MARGIN;
   else
      Hit = Point.x >= x2 - HIT_TEST_MARGIN && Point.x <= x1 + HIT_TEST_MARGIN;

   if (y2 > y1)
      Hit = Hit && (Point.y >= y1 - HIT_TEST_MARGIN && Point.y <= y2 + HIT_TEST_MARGIN);
   else
      Hit = Hit && (Point.y >= y2 - HIT_TEST_MARGIN && Point.y <= y1 + HIT_TEST_MARGIN);

   return Hit;
   }

// *******************************************************************
       bool DLine::Hit_test(GDC& DC, TPoint& Point)  {
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

// -------------------- Executable code section ----------------------

   DC.Set_coord(Coord);

   Point1.Get_pixel(DC, x1, y1);
   Point2.Get_pixel(DC, x2, y2);

   float Denom = x2 - x1;
   if (Denom == 0)
      {
      // Vertical line.

      Hit = (Point.x >= x1 - HIT_TEST_MARGIN && Point.x <= x1 + HIT_TEST_MARGIN &&
             Point.y >= y1 - HIT_TEST_MARGIN && Point.y <= y2 + HIT_TEST_MARGIN);

      }

   else if (Point.x >= x1 && Point.x <= x2)
      {
      // Ok - we're in the ball park.

      float Slope = (y2 - y1) / Denom;

      int y = y1 + (Point.x - x1) * Slope;
      Hit = (Point.y >= y - HIT_TEST_MARGIN && Point.y <= y + HIT_TEST_MARGIN);
      }

   else
      Hit = FALSE;

   return Hit;
   }

// *******************************************************************
       void DLine::Draw_selection(GDC& DC)  {
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
   Draw_handle(DC, Point1);
   Draw_handle(DC, Point2);
   }

// *******************************************************************
       bool DRectangle::Hit_test(GDC& DC, TPoint& Point)  {
// *******************************************************************

//  Short description:
//    Return true if point lies on this object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      int x1,y1,x2,y2;                 // pixel coordinates of start and end of line

// -------------------- Executable code section ----------------------

   DC.Set_coord(Coord);

   Point1.Get_pixel(DC, x1, y1);
   Point2.Get_pixel(DC, x2, y2);

   return Rect_hit_test(Point, x1, y1, x2, y2);
   }

// *******************************************************************
       bool DText::Hit_test(GDC& DC, TPoint& Point)  {
// *******************************************************************

//  Short description:
//    Return true if point lies on this object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      int x1,y1,x2,y2;                 // pixel coordinates of start and end of line

// -------------------- Executable code section ----------------------

   DC.Set_coord(Coord);

   Start_point.Get_pixel(DC, x1, y1);
   End_point.Get_pixel(DC, x2, y2);


   return Rect_hit_test(Point, x1, y1, x2, y2);
   }

// *******************************************************************
      void DText::Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id)  {
// *******************************************************************

//  Short description:
//    Create the popup menu for this object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 0, "&Pen");
   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 1, "&Brush");
   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 2, "&Properties");
   Menu_id += 3;
   }

// *******************************************************************
      bool DText::Do_menu_item
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
       void DText::Properties(TWindow& Parent)  {
// *******************************************************************

//  Short description:
//    Display properties box.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      GString Text_string;             // Text string
      bool Border;                     // Draw a border?

// -------------------- Executable code section ----------------------

   // Get current settings.

   Text_string = Text;
   Border = Draw_box;

   // Execute dialog box.

   TModule igraph(DLL_name);
   if (Text_properties_dialog(&Parent, TEXT_PROP_ID, &igraph,
                              &Text_string, &Font, &Border)
                             .Execute() == IDOK)
      {
      // User pressed ok - save changes

      Text = Text_string;
      Set_draw_box(Border);
      Parent.Invalidate();
      }
   }
#endif
