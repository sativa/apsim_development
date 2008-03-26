#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>

#include <cl\viewbox.h>

DEFINE_RESPONSE_TABLE1(View_box, TListBox)
	 EV_WM_KEYUP,
	 EV_WM_GETDLGCODE,
END_RESPONSE_TABLE;

// *******************************************************************
      View_box::View_box(TWindow *Parent, int Id, TModule *module)
         : TListBox(Parent, Id, module), All_text(10, 0, 10)  {
// *******************************************************************

//  Short description:
//    constructor.

//  Notes:

//  Changes:
//    DPH 28/2/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   }

// *******************************************************************
       void View_box::Read_from_stream(istream& In_stream, char *Delimiter)  {
// *******************************************************************

//  Short description:
//    Read all lines from stream until the delimiter is reached.

//  Notes:

//  Changes:
//    DPH 28/2/95

//  Calls:

//  Internal variables
      bool Found;                      // found the delimiter yet?
      GString Line;                    // Line read from file.

// -------------------- Executable code section ----------------------

   All_text.Flush();
   ClearList();
   do
      {
      Line.read_to_delim(In_stream);
      Line.Replace("\r", "");

      // If delimiter not found on line then store string.

      size_t Pos = Line.find(Delimiter);
      Found = (Pos != NPOS);
      if (!Found)
         {
         All_text.Add(Line);
         AddString("");
         }
      }
   while (In_stream && !Found);

   // Ensure view box will be redrawn.

   Invalidate();
   }

// *******************************************************************
       void View_box::Read_from_array(String_array& Text_array)  {
// *******************************************************************

//  Short description:
//    Copy all strings from text_array to the view_box.

//  Notes:

//  Changes:
//    DPH 28/2/95

//  Calls:

//  Internal variables
      GString Line;                    // Line read from file.

// -------------------- Executable code section ----------------------

   All_text.Flush();
   ClearList();
   for (int Indx = 0;
        Indx < Text_array.GetItemsInContainer();
        Indx++)
      {
      All_text.Add(Text_array[Indx]);
      AddString("");
      }

   // Ensure view box will be redrawn.

   Invalidate();
   }

// *******************************************************************
      void View_box::DrawItem(DRAWITEMSTRUCT far &Draw_info)  {
// *******************************************************************

//  Short description:
//    Draw list box item.  Don't put a border around each item and don't
//    highlight any item.  This gives the appearence of a viewer.

//  Notes:

//  Changes:
//      DPH 23/8/94

//  Calls:                    tbitmap

//  Internal variables

// -------------------- Executable code section ----------------------

   // Create device context object for screen

   TDC Screen_dc(Draw_info.hDC);

   // Set text options.

   Screen_dc.SetBkMode(TRANSPARENT);

   // Calculate a margin

   TSize Size = Screen_dc.GetTextExtent("X", 1);
   int Margin = Size.cx;

   // Display item text.

   TPoint Text_point(Draw_info.rcItem.left + Margin, Draw_info.rcItem.top);
   Screen_dc.SetTextAlign(TA_TOP | TA_LEFT);
   Screen_dc.TextOut(Text_point, All_text[Draw_info.itemID].c_str());
   }

// *******************************************************************
      UINT View_box::EvGetDlgCode(MSG far *)  {
// *******************************************************************

//  Short description:
//    Don't allow dialog box manager to get key presses. We want to process
//    them ourselves.

//  Notes:

//  Changes:
//    DPH 28/2/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

	return (UINT) DLGC_WANTALLKEYS;
	}

// *******************************************************************
       void View_box::EvKeyUp(UINT Key, UINT , UINT )  {
// *******************************************************************

//  Short description:
//    Respond to a key press if necessary.

//  Notes:

//  Changes:
//    DPH 28/2/95

//  Calls:

//  Internal variables
      int Current_index;               // Current index of top element in listbox

// -------------------- Executable code section ----------------------

	// Trap the return key.

	if (Key == VK_DOWN)
		{
      Current_index = GetTopIndex();
      SetTopIndex(Current_index + 1);
      }
   else if (Key == VK_UP)
		{
      Current_index = GetTopIndex();
      if (Current_index > 0)
         SetTopIndex(Current_index - 1);
      }
	}

