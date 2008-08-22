#if !defined(__OBJLIST_H)
#define __OBJLIST_H

#include <classlib\arrays.h>
#include <graph\global.h>
#include <gobjsx\rectdrag.h>

typedef TIArrayAsVector <Drawable_object> Object_array;
enum Front_back_enum  {Front, Back};

// *******************************************************************
       class GRAPH_CLASS_EXPORT Object_list : public TStreamableBase {
// *******************************************************************

//  Short description:
//    Object for encapsulating a list of graphic objects.

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
      Rectangle_drag Rect_drag;        // An instance of a rectangle drag object.
      Object_array Objects;            // list of all objects.
      String_array Object_names;       // List of all names for all objects.
      int Selected_index;              // selected index.
      GDC *DC_ptr;                     // Our dc to play with

//   Protected member functions
       int Find_object(GDC& DC, TPoint& Point);
                                       // Find the object that point lies over.
                                       // Return the object index when found.
       void Check_series_numbering(const char* Series_deleted);
                                       // Check series numbering.


// -------------------------- Public section -------------------------

   public :

//   Public data

//   Public member functions
      Object_list(void);               // constructor
      void Paint(GDC& DC, TRect& DC_dimensions);
                                       // paint the chart on the specified
                                       // device context.

      // ****** SERIES routines ******

      void Add(Drawable_object* Draw_object_ptr,
               Front_back_enum Front_back = Front,
               const char* Object_name = "");
                                       // Add a object to list.  Add at position 2
                                       // because pos 0 and 1 are chart and plot areas.
      void Delete(const char* Object_name);
                                       // Delete an object from the list.
      void Delete_current_object(void);
                                       // delete currently selected object.

      void Delete_all(void);           // Delete all objects.
      void Select(int Item_number = -1, bool Fix_screen = TRUE);
                                       // select the specified item.  -1 indicates
                                       // remove selection.  If Fix screen = TRUE then
                                       // the screen is updated to reflect change
                                       // in selection.
      int Get_select(void)
         {
         return Selected_index;
         };                            // return selection index to caller.

      Drawable_object* Get_object (const char *Object_name)
         {
         int Index = Object_names.Find(GString(Object_name));
         if (Index != INT_MAX)
            return Objects[Index];
         else
            return NULL;
         };
      void Set_coord (Coordinate_system& Coord);
                                       // set all objects coordinate system.
      void Left_button_down(TWindow& Parent, TPoint& Point);
                                       // Left button was pressed - process event.
      void Left_button_up(TPoint& Point);
                                       // Left button was released - process event.
      void Right_button_down(TWindow& Parent, TPoint& Point);
                                       // Right button was pressed - process event.
      void Mouse_move(TPoint& Point);
                                       // Mouse was moved - process event.
      void Command(TWindow& Parent, int id);
                                       // The user clicked on a popup menu.  Go
                                       // perform required action.
      void Paint(GDC& DC);             // Paint all objects.

   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, Object_list, 1);
   };

#endif