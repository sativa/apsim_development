#if !defined(__COLOURS_H)
#define __COLOURS_H

#include <owl\color.h>
#include <cl\ownerdr.h>
#include <cl\transdlg.h>

#define MAX_COLOURS  10
// *******************************************************************
       class GRAPH_CLASS_EXPORT GColours : public GOwner_draw_base,
                               public Transfer_dialog_data  {
// *******************************************************************

//  Short description:
//    Class for encapsulating all allowable colours
//    All colours will be in list by default.

   protected:
      GColour Colour_list[MAX_COLOURS];
                                       // List of all custom colours

   public:
      GColours(void);                  // constructor
      void DrawItem(DRAWITEMSTRUCT far &Draw_info);
                                       // Draw item
      void Set_colour_styles(GColour Colours[],
                             int Num_colours);
                                       // set the colour list.
      void Set_colour_styles_invert(GColour Colours[],
                                    int Num_colours);
                                       // set the colour list - inverted
      void Get_colour_styles(GColour Colours[],
                             int& Num_colours);
                                       // return the colour list.
      GColour Get_selection(void)
         {
         return Colour_list[Selected_item];
         };                            // return selected item.
      void Set_selection(GColour& Colour);
                                       // set selected item.
      void* Get_item (int Item_id);    // return pointer to an item.
      void Remove_item (int Item_id);  // remove item from list.
      void Add_item (void *Data_ptr);  // add item to end of list.
   };

#endif

