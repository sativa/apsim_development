#if !defined(__BRSHSTYS_H)
#define __BRSHSTYS_H

#include <gobjs\intarray.h>
#include <cl\ownerdr.h>
#include <cl\transdlg.h>
// *******************************************************************
       class GRAPH_CLASS_EXPORT GBrush_styles : public GOwner_draw_base,
                                    public Transfer_dialog_data  {
// *******************************************************************

//  Short description:
//    Class for encapsulating all allowable brush styles.
//    All brush styles will be in list by default.


   protected:
      Brush_style_enum  Brush_style_list[MAX_BRUSH_STYLES];
                                       // List of all Brush styles

   public:
      GBrush_styles(void);               // constructor
      void DrawItem(DRAWITEMSTRUCT far &Draw_info);
                                       // Draw item
      void Set_brush_styles(Brush_style_enum Brush_styles[],
                            int Num_brush_styles);
                                       // set the brush style list.
      void Set_brush_styles_invert(Brush_style_enum Brush_styles[],
                                   int Num_brush_styles);
                                       // set the brush style list - inverted
      void Get_brush_styles(Brush_style_enum Brush_styles[],
                            int& Num_brush_styles);
                                       // return the brush style list.
      Brush_style_enum Get_selection(void)
         {
         return Brush_style_list[Selected_item];
         };                            // return selected item.
      void Set_selection(Brush_style_enum& Brush_style);
                                       // set selected item.
      void* Get_item (int Item_id);    // return pointer to an item.
      void Remove_item (int Item_id);  // remove item from list.
      void Add_item (void *Data_ptr);  // add item to end of list.
   };

#endif
