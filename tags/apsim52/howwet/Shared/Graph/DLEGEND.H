#if !defined(__DLEGEND_H)
#define __DLEGEND_H

#include <graph\dseries.h>
// *******************************************************************
       class GRAPH_CLASS_EXPORT DLegend : public DText  {
// *******************************************************************

//  Short description:
//    Object for drawing a legend.

//  Notes:

//  Changes:
//    DPH 21/7/95

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
      void Paint_object(GDC& Device_context);
                                       // Draw the object.
      void Init(void);                 // initialise the object.

// -------------------------- Public section -------------------------

   public :

//   Public data
      DSeries_array* Series_list;      // list of all series objects.

//   Public member functions
      DLegend(void);                   // constructor
      ~DLegend(void);                  // destructor

      #ifdef INTERACTIVE
         void Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id);
                                       // Create the popup menu for this object.
         bool Do_menu_item (TWindow& Parent, int& Menu_id, int User_selected_id);
                                       // If the user_selected_id belongs to us
                                       // then return TRUE and perform appropriate
                                       // actions.
         void Properties(TWindow& Parent);
                                       // Display properties dialog box.

      #endif

      DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, DLegend, 1);
   };

#endif
