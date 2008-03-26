#include <graph\global.h>
#pragma hdrstop
#include <graph\dobject.h>
#include <graph\dlegend.h>

IMPLEMENT_CASTABLE1( DLegend, DText );
IMPLEMENT_STREAMABLE1( DLegend, DText );

// *******************************************************************
      void DLegend::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of DLegend to the passed opstream.

//  Notes:

//  Changes:
//      DPH 28/9/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (DText*) GetObject(), os);

   }

// *******************************************************************
      void *DLegend::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of DLegend from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 28/9/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   GetObject()->Init();
   ReadBaseObject( (DText*) GetObject(), is);

   return GetObject();
   }

// *******************************************************************
      DLegend::DLegend(void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Init();

   // make legend a bit lower than title so that it is visible.

   GPoint Tie_point;
   Tie_point.Set_world(10000, 500);
   Set_position(Tie_point,
                TA_RIGHT,
                TA_TOP);
   }

// *******************************************************************
      DLegend::~DLegend(void)  {
// *******************************************************************

//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   delete Series_list;
   }

// *******************************************************************
      void DLegend::Init(void)  {
// *******************************************************************

//  Short description:
//    initialise the object.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Set_x_margin(8);                    // Set a sample size of 8 mm
   Font.Set_size(8);                // 8 points high.
   Draw_box = TRUE;
   Brush.Set_style(BS_Solid);
   Series_list = new DSeries_array(10, 0, 10);
   Series_list->OwnsElements(FALSE);
   }


// *******************************************************************
       void DLegend::Paint_object(GDC& DC)  {
// *******************************************************************

//  Short description:
//    Draw the legend on the specified device context.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
      int Row = 1;                     // Current row number
      int Col = 1;                     // Current col number
      GPoint Start_point;              // Start point for sample
      GPoint End_point;                // End point for sample
      TRect Rect;                      // Rectangle structure for dseries objects

// -------------------- Executable code section ----------------------

   Text = "";
   for (int Series_number = 0;
        Series_number < Series_list->GetItemsInContainer();
        Series_number++)
      {
      Text += (*Series_list)[Series_number]->Name + "\n";
      }

   DText::Paint_object(DC);

   // We need the dimensions of a text character.

   TSize Size = DC.GetTextExtent("X", 1);

   // Put in our legend samples.

   for (int Line_num = 0;
        Line_num < Series_list->GetItemsInContainer();
        Line_num++)
      {
      Goto_row_col(DC, Row, Col, Start_point);

      // Subtract our x margin.

      Start_point.Add_mm(-X_margin, 0);

      // Create an end point.

      End_point = Start_point;
      End_point.Add_mm(X_margin, 0);
      End_point.Add_pixel(0, Size.cy);
      End_point.Add_pixel(-Size.cx, 0);

      // Fill a rectangle structure.

      Start_point.Get_pixel(DC, Rect.left, Rect.top);
      End_point.Get_pixel(DC, Rect.right, Rect.bottom);

      // Pass rectangle to series object to allow it to draw it's sample.

      (*Series_list)[Line_num]->Draw_series_sample(DC, Rect);

      // Increment Row and Col if necessary

      Row++;
      if (Row > Num_of_rows)
         {
         Row = 1;
         Col++;
         }
      }
   }


