#include <graph\global.h>
#pragma hdrstop
#include <graph\dobject.h>
#include <graph\dlegend.h>

#include <graph\igraph.rh>

#include <owl\dialog.h>
#include <owl\scrollba.h>
#include <owl\static.h>
#include <owl\edit.h>
#include <owl\combobox.h>

// *******************************************************************
       class Legend_properties_dialog : public TDialog {
// *******************************************************************

//  Short description:
//      legend properties dialog class.

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
      TComboBox *Position_control;     // position control
      TEdit *Line_spacing_control;     // line spacing control
      TComboBox *Num_rows_control;     // number of rows control
      TComboBox *Num_cols_control;     // Number of cols control
      TStatic *Sample_size_static;     // Sample size static control
      TScrollBar *Sample_size_scroll;  // Sample size scroll bar

      DText::Position_enum *Horiz_position_ptr;
                                       // Storage for selected horizontal position
      DText::Position_enum *Vert_position_ptr;
                                       // Storage for selected vertical position
      float *Spacing_ptr;              // Storage for line spacing
      int *Rows_ptr;                   // Storage for num rows
      int *Cols_ptr;                   // Storage for num cols
      GFont *Font_ptr;                 // Storage for font
      int *Sample_ptr;                 // Storage for sample size

//   Protected member functions
       void SetupWindow(void);         // setup window
       void Ok_button(void);           // Ok button was pressed.
       void EvVScroll(UINT Scroll_code, UINT Thumb_pos, HWND Window_control);
                                       // A scroll bar was changed.
       void Font_button(void);         // font button was clicked.
       void Fill_combo_box(TComboBox *Combo_control, int Index);
                                       // Fill the specified combobox and select
                                       // the appropriate item.


// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      Legend_properties_dialog(TWindow* Parent, int Resource_id, TModule *Module);
                                       // constructor
      void Setup(DText::Position_enum& Horiz_position,
                 DText::Position_enum& Vert_position,
                 float& Spacing,
                 int& Rows,
                 int& Cols,
                 GFont& Font,
                 int& Sample);         // setup dialog.

      DECLARE_RESPONSE_TABLE(Legend_properties_dialog);
   };

DEFINE_RESPONSE_TABLE1(Legend_properties_dialog, TDialog)
   EV_WM_VSCROLL,
   EV_BN_CLICKED(IDC_PUSHBUTTON1, Font_button),
   EV_BN_CLICKED(IDOK, Ok_button),
END_RESPONSE_TABLE;

// *******************************************************************
       Legend_properties_dialog::Legend_properties_dialog
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

   Position_control = new TComboBox(this, IDC_COMBOBOX1);
   Line_spacing_control = new TEdit(this, IDC_EDIT1, 10);
   Num_rows_control = new TComboBox(this, IDC_COMBOBOX2);
   Num_cols_control = new TComboBox(this, IDC_COMBOBOX3);
   Sample_size_static = new TStatic(this, IDC_STATIC1, 10);
   Sample_size_scroll = new TScrollBar(this, IDC_VSCROLL1);
   }

// *******************************************************************
      void Legend_properties_dialog::Setup(
                 DText::Position_enum& Horiz_position,
                 DText::Position_enum& Vert_position,
                 float& Spacing,
                 int& Rows,
                 int& Cols,
                 GFont& Font,
                 int& Sample)  {
// *******************************************************************

//  Short description:
//    setup dialog

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Horiz_position_ptr = &Horiz_position;
   Vert_position_ptr = &Vert_position;
   Spacing_ptr = &Spacing;
   Rows_ptr = &Rows;
   Cols_ptr = &Cols;
   Font_ptr = &Font;
   Sample_ptr = &Sample;
   }

// *******************************************************************
       void Legend_properties_dialog::SetupWindow(void)  {
// *******************************************************************

//  Short description:
//    setup window

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      int Index;                       // Index used for selected listbox entries
      GString Text;                    // text string for edit controls.

// -------------------- Executable code section ----------------------

   TDialog::SetupWindow();

   // Set position combo box.

   Position_control->AddString("Auto");
   Position_control->AddString("Top left");
   Position_control->AddString("Top middle");
   Position_control->AddString("Top right");
   Position_control->AddString("Middle left");
   Position_control->AddString("Middle");
   Position_control->AddString("Middle right");
   Position_control->AddString("Bottom left");
   Position_control->AddString("Bottom middle");
   Position_control->AddString("Bottom right");
   switch (*Vert_position_ptr)
      {
      case DText::top : Index = 1;    break;
      case DText::middle : Index = 4; break;
      case DText::bottom : Index = 7; break;
      }
   switch (*Horiz_position_ptr)
      {
      case DText::middle : Index++;    break;
      case DText::right  : Index += 2; break;
      }
   Position_control->SetSelIndex(Index);

   // set line spacing edit control.

   Text.Set_real(*Spacing_ptr);
   Line_spacing_control->SetText(Text.c_str());

   // set the number of rows combo box.

   Fill_combo_box(Num_rows_control, *Rows_ptr);

   // set the number of rows combo box.

   Fill_combo_box(Num_cols_control, *Cols_ptr);

   // set the sample size static and scroll bars.

   Sample_size_scroll->SetRange(0, 30);
   Sample_size_scroll->SetPosition(*Sample_ptr);

   EvVScroll(0, 0, *Sample_size_scroll);
   }

// *******************************************************************
       void Legend_properties_dialog::Ok_button(void)  {
// *******************************************************************

//  Short description:
//    Ok button was pressed.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      int Index;                       // Index used for selected listbox entries
      GString Text;                    // text string for edit controls.
      char Text_str[30];

// -------------------- Executable code section ----------------------

   CloseWindow(IDOK);

   // get position specified by user.

   Index = Position_control->GetSelIndex();
   switch (Position_control->GetSelIndex())
      {
      case 1 :
      case 2 :
      case 3 : *Vert_position_ptr = DText::top;
               break;
      case 4 :
      case 5 :
      case 6 : *Vert_position_ptr = DText::middle;
               Index -= 3;
               break;
      case 7 :
      case 8 :
      case 9 : *Vert_position_ptr = DText::bottom;
               Index -= 6;
               break;
      }
   switch (Index)
      {
      case 1 : *Horiz_position_ptr = DText::left;   break;
      case 2 : *Horiz_position_ptr = DText::middle; break;
      case 3 : *Horiz_position_ptr = DText::right;  break;
      }

   // Get line spacing

   Line_spacing_control->GetText(Text_str, sizeof(Text_str));
   Text = Text_str;
   *Spacing_ptr = Text.Get_real();

   // Get num rows and columns

   *Rows_ptr = Num_rows_control->GetSelIndex();
   *Cols_ptr = Num_cols_control->GetSelIndex();

   // Get sample size.

   *Sample_ptr = Sample_size_scroll->GetPosition();
   }

// *******************************************************************
       void Legend_properties_dialog::EvVScroll
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

   wsprintf(Text, "%i mm", Sample_size_scroll->GetPosition());
   Sample_size_static->SetText(Text);
   }

// *******************************************************************
       void Legend_properties_dialog::Fill_combo_box
          (TComboBox *Combo_control, int Index)  {
// *******************************************************************

//  Short description:
//    Fill the specified combobox and select the appropriate item.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      GString Item;                    // Item text to add to combo box.

// -------------------- Executable code section ----------------------

   Combo_control->AddString("Auto");
   for (int Item_num = 1;
        Item_num <= 20;
        Item_num++)
      {
      Item.Set_integer(Item_num);
      Combo_control->AddString(Item.c_str());
      }

   Combo_control->SetSelIndex(Index);
   }


// *******************************************************************
       void Legend_properties_dialog::Font_button(void)  {
// *******************************************************************

//  Short description:
//    Font button was clicked.

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
      void DLegend::Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id)  {
// *******************************************************************

//  Short description:
//    Create the popup menu for this object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 0, "&Legend pen");
   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 1, "&Legend brush");
   Popup_menu.AppendMenu(MF_ENABLED, Menu_id + 2, "&Legend properties");
   Menu_id += 3;
   }

// *******************************************************************
      bool DLegend::Do_menu_item
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
      void DLegend::Properties(TWindow& Parent)  {
// *******************************************************************

//  Short description:
//    Display properties dialog box.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   TModule igraph(DLL_name);
   Legend_properties_dialog Legend_dlg(&Parent, LEGEND_PROP_ID, &igraph);
   Legend_dlg.Setup(Horiz_pos, Vert_pos, Line_height,
                    Num_rows, Num_columns, Font, X_margin);

   if (Legend_dlg.Execute() == IDOK)
      Parent.Invalidate();
   }

#endif
