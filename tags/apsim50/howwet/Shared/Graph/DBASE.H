#if !defined(__DBASE_H)
#define __DBASE_H


// *******************************************************************
       class GRAPH_CLASS_EXPORT Drawable_object : public Interactive_object  {
// *******************************************************************

//  Short description:
//    Base class for generic drawable objects.
//    All drawable graphic objects are derived from this one.

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
      bool Is_visible;                 // Is object visible?
      Coordinate_system Coord;         // Coordinate system for this object.

//   Protected member functions
      virtual void Paint_object(GDC& DC) {};
                                       // Go paint the object

// -------------------------- Public section -------------------------

   public :

//   Public data
      GPen Pen;
      GBrush Brush;

//   Public member functions
      Drawable_object(void);           // constructor
      int operator==(const Drawable_object& )
         {
         return FALSE;
         }
      void Paint(GDC& Device_context);
                                       // Draw the object.
      // ****** GET Routines ******

      bool Get_is_visible(void)
         {
         return Is_visible;
         };                            // Return TRUE if object is visible

      // ****** SET Routines ******

      virtual void Set_coord(Coordinate_system& C)
         {
         Coord = C;
         };                            // Setup the coordinate system.
      void Set_visible(int Visible = TRUE)
         {
         Is_visible = Visible;
         };                            // Set the visibility of object.

      #ifdef INTERACTIVE
         void Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id);
                                       // Create the popup menu for this object.
         bool Do_menu_item (TWindow& Parent, int& Menu_ID, int User_selected_id);
                                       // If the user_selected_id belongs to us
                                       // then return TRUE and perform appropriate
                                       // actions.
      #endif

   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, Drawable_object, 1);
   };

#endif
