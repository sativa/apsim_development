#include <consts.h>
#include <graph\global.h>

#include <graph\dchart.h>
#include <owl\printer.h>


// *******************************************************************
       class GRAPH_CLASS_EXPORT Chart_printout : public TPrintout  {
// *******************************************************************

//  Short description:
//    Printout object for charts

//  Notes:

//  Changes:
//    DPH 9/1/96

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      TWindow * *Chart_objects;        // List of chart windows to print
      int Max_charts;                  // Maximum number of charts
      int Num_charts;                  // Number of charts stored so far.
      int First_chart;                 // First chart to print
      int Last_chart;                  // Last chart to print
      int Charts_per_page;             // Charts to print per page.
      String_array *Header_strings;    // header strings.

//   Protected member functions
      void Adjust_print_area(TRect& Print_area,
                             int Chart_position);
                                       // Adjust the print area for the specified
                                       // chart position (starts at position 1)
      int Num_pages(void)
         {
         return (Last_chart - First_chart) / Charts_per_page + 1;
         };                            // return number of pages required.
      void Print_headers(int Text_height);
                                       // print headers.

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      Chart_printout(const char *title, int Max_num_charts);
                                       // constructor
      ~Chart_printout(void);           // destructor
      void GetDialogInfo(int& minPage, int& maxPage,
                         int& selFromPage, int& selToPage);
                                       // return page info to caller.
		void PrintPage(int , TRect& , unsigned );
                                 		// print the page on the printer
      void SetBanding(bool b)  {Banding = b;}
                                       // set banding variable
      bool HasPage(int Page_number)  {return Page_number <= Num_pages();};
                                       // further pages to go?

      void Add_chart(TWindow *Chart_window_ptr);
                                       // Add specified chart window to list.
      void Set_chart_print_range (int Start_chart,
                                  int End_chart,
                                  int Num_charts_per_page)
         {
         First_chart = Start_chart;
         Last_chart = End_chart;
         Charts_per_page = Num_charts_per_page;
         }
      void Set_header_strings (String_array* Strings)
         {
         Header_strings = Strings;
         }
	};

