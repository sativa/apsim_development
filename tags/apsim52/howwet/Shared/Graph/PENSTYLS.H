#if !defined(__PENSTYLS_H)
#define __PENSTYLS_H

#include <graph\dbase.h>
#include <cl\ownerdr.h>
#include <cl\transdlg.h>
// *******************************************************************
       class GRAPH_CLASS_EXPORT GPen_styles : public GOwner_draw_base,
                                  public Transfer_dialog_data  {
// *******************************************************************

//  Short description:
//    Class for encapsulating all allowable pen styles.
//    All pen styles will be in list by default.

   protected:
      Pen_style_enum Pen_style_list[MAX_PEN_STYLES];
                                       // List of all pen styles

   public:
      GPen_styles(void);               // constructor
      void Set_pen_styles(Pen_style_enum Pen_styles[],
                          int Num_pen_styles);
                                       // set the pen style list.
      void Set_pen_styles_invert(Pen_style_enum Pen_styles[],
                                 int Num_pen_styles);
                                       // set the list to the invert of
                                       // the specified list.
      void Get_pen_styles(Pen_style_enum Pen_styles[],
                          int& Num_pen_styles);
                                       // return the pen style list.
      void DrawItem(DRAWITEMSTRUCT far &Draw_info);
                                       // Draw item
      Pen_style_enum Get_selection(void)
         {
         return Pen_style_list[Selected_item];
         };                            // return selected item.
      void Set_selection(Pen_style_enum& Pen_style);
                                       // set selected item.
      void* Get_item (int Item_id);    // return pointer to an item.
      void Remove_item (int Item_id);  // remove item from list.
      void Add_item (void *Data_ptr);  // add item to end of list.
   };



#endif
