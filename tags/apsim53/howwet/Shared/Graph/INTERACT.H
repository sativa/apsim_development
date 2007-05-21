#if !defined(__INTERACT_H)
#define __INTERACT_H


#define MENU_START_ID   1000

#include <owl\window.h>
#include <owl\gdiobjec.h>
#include <owl\menu.h>

class GRAPH_CLASS_EXPORT GDC;
// *******************************************************************
       class GRAPH_CLASS_EXPORT Interactive_object : public TStreamableBase {
// *******************************************************************

//  Short description:
//    Base class for generic interactive objects.  All interactive graphic objects
//    are derived from this one.

//  Notes:

//  Changes:
//    DPH 10/4/95

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      bool Is_selected;                // TRUE if this object is selected.
      bool Is_selectable;              // Is object selectable?
      bool Is_movable;                 // Is object moveable?
      bool Is_sizable;                 // Is object sizable?

      bool Sizing;                     // Is object currently being re-sized?
      bool Moving;                     // Is object currently being moved?

//   Protected member functions
      void Draw_handle(GDC& DC, GPoint& Point);
                                       // Draw a selection handle at specified
                                       // point.

// -------------------------- Public section -------------------------

   public :

//   Public data

//   Public member functions
      Interactive_object(void);        // constructor

      // ****** GET routines ******

      bool Get_is_selected(void)
         {
         return Is_selected;
         };                            // Return TRUE if object is selected
      bool Get_selectable(void)
         {
         return Is_selectable;
         };                            // Return TRUE if object is selectable.
      bool Get_movable(void)
         {
         return Is_movable;
         };                            // Return TRUE if object is movable.
      bool Get_sizable(void)
         {
         return Is_sizable;
         };                            // Return TRUE if object is sizable.

      // ****** SET routines ******

      void Set_selected(int Selected = TRUE)
         {
         Is_selected = Selected;
         };                            // Select this object.
      void Set_selectable(int Selectable = TRUE)
         {
         Is_selectable = Selectable;
         };                            // Set the Is_selectable flag for this object.
      void Set_movable(int Movable = TRUE)
         {
         Is_movable = Movable;
         };                            // Set the Is_movable flag for this object.
      void Set_sizable(int Sizable = TRUE)
         {
         Is_sizable = Sizable;
         };                            // Set the Is_sizable flag for this object.

      // ****** Derivable functions ******

      virtual void Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id) {};
                                       // Create the popup menu for this object.
      virtual bool Do_menu_item (TWindow& Parent, int& Menu_ID, int User_selected_id) {return FALSE;};
                                       // If the user_selected_id belongs to us
                                       // then return TRUE and perform appropriate
                                       // actions.
      virtual bool Hit_test(GDC &DC, TPoint& Point) {return FALSE;};
                                       // Return TRUE if user has clicked on
                                       // this object.
      virtual void Properties(TWindow& Parent)  {};
                                       // Display properties box and allow user
                                       // to change settings.
      virtual void Draw_selection(GDC& DC);
                                       // The object is selected.  Draw handles
                                       // to indicate selection.
      virtual void Get_bounding_rectangle(GDC& DC, TRect& Current_position)
         {
         Current_position.left = 0;
         Current_position.right = 0;
         };
                                       // Return the bounding rectangle to caller.
      virtual void Set_bounding_rectangle(GDC& DC, TRect& Current_position) {};
                                       // The user has modified the bounding rectangle.
                                       // Reset objects position.
   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, Interactive_object, 1);
   };


#endif
