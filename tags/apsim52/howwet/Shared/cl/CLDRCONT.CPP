#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include <cl\cldrcont.h>


// *******************************************************************
       Calender_control::Calender_control
          (TWindow *Parent, int Res_id, Calender *Calender_p, TModule *module)
          : TControl(Parent, Res_id, module)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 9/3/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   First_update = TRUE;
   Calender_ptr = Calender_p;
   }

// *******************************************************************
       Calender_control::~Calender_control(void)  {
// *******************************************************************

//  Short description:
//    destructor.

//  Notes:

//  Changes:
//    DPH 9/3/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   }

// *******************************************************************
       void Calender_control::DrawItem(DRAWITEMSTRUCT far &Draw_info)  {
// *******************************************************************

//  Short description:
//    Draw the item in the dialog box.

//  Notes:

//  Changes:
//    DPH 9/3/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   if (First_update)
      {
      Invalidate();
      First_update = FALSE;
      }
   else
      {
      First_update = TRUE;

      // Create device context object for screen

      TDC Screen_dc(Draw_info.hDC);

      // Draw the calender in window

      TRect Draw_rectangle = Draw_info.rcItem;
      Calender_ptr->Draw(Screen_dc, Draw_rectangle);
      }
   }

// *******************************************************************
       Calender_printout::Calender_printout
          (const char *Title,
           Calender_control *Calender_control_p)
      	: TPrintout(Title)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 9/3/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

	Calender_control_ptr = Calender_control_p;
   Header_strings = NULL;
	}

// *******************************************************************
       void Calender_printout::PrintPage(int , TRect& , unsigned )  {
// *******************************************************************

//  Short description:
//    Print the calender on the printer

//  Notes:

//  Changes:
//    DPH 9/3/95

//  Calls:

//  Internal variables
      DRAWITEMSTRUCT Draw_info;        // Draw info object

// -------------------- Executable code section ----------------------

   Draw_info.hDC = *DC;

   // setup the rcItem field of Draw_info

   ::GetClipBox(*DC, &Draw_info.rcItem);

   // Print the headers.

   int Text_height = DC->GetTextExtent("X", 1).cy;
   int Num_headers = 0;
   if (Header_strings != NULL)
      {
      Print_headers(Text_height);
      Num_headers = Header_strings->GetItemsInContainer();
      }

   // Adjust the printing area for the current chart.

   Draw_info.rcItem.top = 1 + Num_headers * Text_height;

   // paint the calender

   Calender_control_ptr->DrawItem(Draw_info);
   Calender_control_ptr->DrawItem(Draw_info);
	}

// *******************************************************************
       void Calender_printout::Print_headers(int Text_height)  {
// *******************************************************************

//  Short description:
//    Print header strings.

//  Notes:

//  Changes:
//    DPH 9/1/96

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   for (int String_number = 0;
        String_number < Header_strings->GetItemsInContainer();
        String_number++)
      {
      DC->TextOut (1, 1 + String_number * Text_height,
                   (*Header_strings)[String_number].c_str());
      }
   }

