#include <graph\global.h>
#pragma hdrstop

#include <owl\choosefo.h>

#include <graph\igraph.rh>
#include <graph\colours.h>
#include <graph\penstyls.h>
#include <graph\penwidts.h>
#include <graph\brshstys.h>
#include <cl\ownerdr.h>

// *******************************************************************
       class Pen_properties_dialog : public TDialog {
// *******************************************************************

//  Short description:
//      Pen properties dialog class.

//  Notes:

//  Changes:
//      DPH 8/8/95

// ------------------------- Protected section -----------------------

   protected :

//   Protected data

//   Protected member functions

// -------------------------- Public section -------------------------

   public :

//   Public data
      GColours               Colours;
      GPen_styles            Pen_styles;
      GPen_width_owner_draw  Pen_widths;

//   Public member functions
      Pen_properties_dialog(TWindow* Parent, int Resource_id, TModule *Module);
                                       // constructor
   };

// *******************************************************************
       Pen_properties_dialog::Pen_properties_dialog
          (TWindow*           Parent,
           int                Resource_id,
           TModule*           Module)
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

   new GOwner_draw_combo (this,
                          IDC_COMBOBOX1,
                          NULL,
                          &Colours);
   new GOwner_draw_combo (this,
                          IDC_COMBOBOX2,
                          NULL,
                          &Pen_styles);
   new GOwner_draw_combo (this,
                          IDC_COMBOBOX3,
                          NULL,
                          &Pen_widths);
   }

// *******************************************************************
       class Brush_properties_dialog : public TDialog {
// *******************************************************************

//  Short description:
//      Brush properties dialog class.

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

//   Protected member functions

// -------------------------- Public section -------------------------

   public :

//   Public data
      GColours               Colours;
      GBrush_styles          Brush_styles;

//   Public member functions
      Brush_properties_dialog(TWindow* Parent, int Resource_id, TModule *Module);
                                       // constructor

   };

// *******************************************************************
       Brush_properties_dialog::Brush_properties_dialog
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

   new GOwner_draw_combo (this,
                          IDC_COMBOBOX1,
                          NULL,
                          &Colours);
   new GOwner_draw_combo (this,
                          IDC_COMBOBOX2,
                          NULL,
                          &Brush_styles);
   }

#ifdef INTERACTIVE

// *******************************************************************
       void GPen::Properties(TWindow& Parent)  {
// *******************************************************************

//  Short description:
//    Display properties dialog box.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
      GColour Colour;                  // Current colour of pen
      Pen_style_enum Style;            // current style of pen
      int Width;                       // current weight of pen

// -------------------- Executable code section ----------------------

   // Get current pen settings.

   Get_colour(Colour);
   Get_style(Style);
   Get_width(Width);

   // Execute dialog box.

   TModule igraph(DLL_name);
   Pen_properties_dialog Pen_dlg(&Parent,
                                 PEN_PROP_ID,
                                 &igraph);
   Pen_dlg.Colours.Set_selection(Colour);
   Pen_dlg.Pen_styles.Set_selection(Style);
   Pen_dlg.Pen_widths.Set_selection(Width);

   if (Pen_dlg.Execute() == IDOK)
      {
      // User pressed ok - save changes to pen.

      Set_colour(Pen_dlg.Colours.Get_selection());
      Set_style(Pen_dlg.Pen_styles.Get_selection());
      Set_width(Pen_dlg.Pen_widths.Get_selection());
      Parent.Invalidate();
      }
   }

// *******************************************************************
       void GBrush::Properties(TWindow& Parent)  {
// *******************************************************************

//  Short description:
//    Display properties dialog box.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
      GColour Colour;                  // Current colour of brush
      Brush_style_enum Style;          // current style of brush

// -------------------- Executable code section ----------------------

   // Get current pen settings.

   Get_colour(Colour);
   Get_style(Style);

   // Execute dialog box.

   TModule igraph(DLL_name);
   Brush_properties_dialog Brush_dlg(&Parent,
                                     BRUSH_PROP_ID,
                                     &igraph);
   Brush_dlg.Colours.Set_selection(Colour);
   Brush_dlg.Brush_styles.Set_selection(Style);

   if (Brush_dlg.Execute() == IDOK)
      {
      // User pressed ok - save changes to pen.

      Set_colour(Brush_dlg.Colours.Get_selection());
      Set_style(Brush_dlg.Brush_styles.Get_selection());
      Parent.Invalidate();
      }
   }

// *******************************************************************
       void GFont::Properties(TWindow& Parent)  {
// *******************************************************************

//  Short description:
//    Display properties dialog box.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
      TChooseFontDialog::TData Font_data;
                                       // Our font data.

// -------------------- Executable code section ----------------------

   TModule igraph(DLL_name);

   memset(&Font_data, 0, sizeof(TChooseFontDialog::TData));

   Font_data.DC = GetDC(Parent);
   Font_data.Flags = CF_INITTOLOGFONTSTRUCT | CF_BOTH | CF_EFFECTS;
   Font_data.LogFont = Font_info;

   if (TChooseFontDialog(&Parent, Font_data, 0, 0, &igraph).Execute() == IDOK)
      {
      Font_info = Font_data.LogFont;
      }

   ReleaseDC(Parent, Font_data.DC);
   Parent.Invalidate();
   }
#endif
