#include <gobjsx\calender.h>
#include <owl\printer.h>

// *******************************************************************
       class huge Calender_control : public TControl  {
// *******************************************************************

//  Short description:
//    Control object for displaying a calender

//  Notes:

//  Changes:
//    DPH 9/3/95

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      Calender *Calender_ptr;          // pointer to calender object
      bool First_update;               // Is this the first update ?

//   Protected member functions

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      Calender_control(TWindow *Parent, int Res_id, Calender* Calender_ptr,
                       TModule *module = 0);
                                       // constructor
      ~Calender_control(void);            // destructor
      void DrawItem(DRAWITEMSTRUCT far &Draw_info);
                                       // Draw the item in the dialog box.
   };

// *******************************************************************
       class Calender_printout : public TPrintout  {
// *******************************************************************

//  Short description:
//    Printout object for graph

//  Notes:

//  Changes:
//    DPH 9/3/95

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      Calender_control *Calender_control_ptr;// pointer to calender control object
      String_array *Header_strings;          // header strings.

//   Protected member functions
      void Print_headers(int Text_height);
                                       // print all headers.

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
		Calender_printout(const char *title, Calender_control *Graph_cont_ptr);
                                       // constructor
		void PrintPage(int , TRect& , unsigned );
                                 		// print the page on the printer
      void Set_header_strings(String_array* Header_list)
         {
         Header_strings = Header_list;
         };                            // set header strings.
	};

