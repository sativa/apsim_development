#if !defined(__PENWIDTS_H)
#define __PENWIDTS_H

#define MAX_PEN_WIDTHS  5

#include <cl\ownerdr.h>
#include <cl\transdlg.h>
// *******************************************************************
       class GRAPH_CLASS_EXPORT GPen_width_owner_draw : public GOwner_draw_base,
                                            public Transfer_dialog_data  {
// *******************************************************************

//  Short description:
//    Class for encapsulating an pen width owner draw object.
//    Has all pen widths by default in list.

   protected:
      int Pen_width_list[MAX_PEN_WIDTHS];
                                       // List of all pen widths

   public:
      GPen_width_owner_draw(void);     // constructor
      void Set_pen_widths(int Pen_widths[], int Num_pen_widths);
                                       // set the pen width list.

      void DrawItem(DRAWITEMSTRUCT far &Draw_info);
                                       // Draw item
      int Get_selection(void)
         {
         return Pen_width_list[Selected_item];
         };                            // return selected item.
      void Set_selection(int Pen_width);
                                       // set selected item.
      void* Get_item (int Item_id);    // return pointer to an item.
      void Remove_item (int Item_id);  // remove item from list.
      void Add_item (void *Data_ptr);  // add item to end of list.
   };


#endif
