#include <gobjs\gstring.h>
#include <iostream.h>
#include <owl\listbox.h>

// *******************************************************************
       class huge View_box : public TListBox  {
// *******************************************************************

//  Short description:
//    This class is a simple text viewer class derived from a listbox.
//    It is a read only control allowing the user to scroll through
//    some text.

//  Notes:

//  Changes:
//    DPH 28/2/95

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      String_array All_text;           // Array of all text lines to display.

//   Protected member functions
      void DrawItem(DRAWITEMSTRUCT far &Draw_info);
                                       // Draw list box item.  Don't put a
                                       // border around each item and don't
                                       // highlight any item.  This gives the
                                       // appearence of a viewer.
      UINT EvGetDlgCode(MSG far *);    // Don't allow dialog box manager to get
                                       // key presses. We want to process
                                       // them ourselves.
      void EvKeyUp(UINT Key, UINT , UINT );
                                       // Respond to a key press if necessary.

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      View_box(TWindow *Parent, int Id, TModule *module = 0);
                                       // constructor.
      void Read_from_stream(istream& In_stream, char *Delimiter = 0);
                                       // Read all lines from stream until the
                                       // delimiter is reached.
      void Read_from_array(String_array& Text_array);
                                       // Copy all strings from text_array to
                                       // the view_box.
      DECLARE_RESPONSE_TABLE(View_box);
   };
