#if !defined(__GPOINT_H)
#define __GPOINT_H

// *******************************************************************
//     file gpoint.h
// *******************************************************************

//  Short description:
//    This module provides a generic point class that is used throughout
//    graphlib2.

//  Notes:

//  Changes:
//      DPH 20/7/95

// ----------------------- Declaration section -----------------------

//  Include files


//  Constants
#define PIXEL_POINT     0
#define MM_POINT        1
#define WORLD_POINT     2
#define LOGICAL_POINT   3

class GRAPH_CLASS_EXPORT GDC;

// *******************************************************************
       class GRAPH_CLASS_EXPORT GPoint : public TStreamableBase {
// *******************************************************************

//  Short description:
//      Point class for defining a single point in 2 dimensions.

//  Notes:

//  Changes:
//      DPH 20/7/95

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      int Pixel_x;
      int Pixel_y;                     // coordinates of our point (pixels)
      int mm_x;
      int mm_y;                        // coordinates of our point (mm)
      int World_x;
      int World_y;                     // coordinates of our point (world)
      float Logical_x;
      float Logical_y;
      Axis_type_enum x_logical_type;
      Axis_type_enum y_logical_type;
      bool Data_in_logical;            // do we have any data in the logical coordinates?
      bool Data_in_world;              // do we have any data in the world coordinates?

//   Protected member functions
      void Reset(void);                // Reset object

// -------------------------- Public section -------------------------

   public :

//   Public data

//   Public member functions
      GPoint();                        // constructor
      ~GPoint();                       // destructor
      void Set_pixel(int X, int Y);    // set the x and y coordinates (PIXELS)
      void Set_mm(int X, int Y);       // set the x and y coordinates (MM)
      void Set_world(int X, int Y);    // set the x and y coordinates (WORLD)
      void Set_logical(float X, float Y, Axis_type_enum x_type, Axis_type_enum y_type);
                                       // set the x and y coordinates (LOGICAL)
      void Add_pixel(int X, int Y);    // add the x and y coordinates (PIXELS)
      void Add_mm(int X, int Y);       // add the x and y coordinates (MM)
      void Add_world(int X, int Y);    // add the x and y coordinates (WORLD)
      void Add_logical(float X, float Y, Axis_type_enum x_type, Axis_type_enum y_type);
                                       // add the x and y coordinates (LOGICAL)
      void Get_pixel(GDC& DC, long& X, long& Y);
                                       // get the x and y coordinates (PIXELS)
      void Get_mm(int& X, int& Y)
         {
         X = mm_x;
         Y = mm_y;
         };                            // get the x and y coordinates (MM)
      void Get_world(int& X, int& Y)
         {
         X = World_x;
         Y = World_y;
         };                            // get the x and y coordinates (WORLD)
      void Get_logical(float X, float Y)
         {
         X = Logical_x;
         Y = Logical_y;
         };                            // get the x and y coordinates (LOGICAL)

      GPoint& operator+(const GPoint& size)
         {
         Add_pixel(size.Pixel_x, size.Pixel_y);
         Add_mm(size.mm_x, size.mm_y);
         Add_world(size.World_x, size.World_y);
         if (size.Data_in_logical)
            Add_logical(size.Logical_x, size.Logical_y,
                        size.x_logical_type, size.y_logical_type);

         return *this;
         }
   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, GPoint, 1);
   };

#endif