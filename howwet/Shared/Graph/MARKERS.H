#if !defined(__MARKERS_H)
#define __MARKERS_H

#include <cl\ownerdr.h>
#include <graph\dbase.h>
#include <graph\dseries.h>
#include <cl\transdlg.h>
// *******************************************************************
       class GRAPH_CLASS_EXPORT GMarkers : public GOwner_draw_base,
                               public Transfer_dialog_data  {
// *******************************************************************

//  Short description:
//    Class for encapsulating all marker styles.
//    All marker styles will be in list by default.

   protected:
      Marker_type_enum Marker_list[MAX_MARKERS];
                                       // List of all custom markers

   public:
      GMarkers(void);                  // constructor
      void DrawItem(DRAWITEMSTRUCT far &Draw_info);
                                       // Draw item
      void Set_marker_styles(Marker_type_enum Marker_styles[],
                             int Num_marker_styles);
                                       // set the marker style list.
      void Set_marker_styles_invert(Marker_type_enum Marker_styles[],
                                    int Num_marker_styles);
                                       // set the marker style list - inverted
      void Get_marker_styles(Marker_type_enum Marker_styles[],
                             int& Num_marker_styles);
                                       // return all markers in list to caller.

      Marker_type_enum Get_selection(void)
         {
         return Marker_list[Selected_item];
         };                            // return selected item.
      void Set_selection(Marker_type_enum& Marker_style);
                                       // set selected item.
      void* Get_item (int Item_id);    // return pointer to an item.
      void Remove_item (int Item_id);  // remove item from list.
      void Add_item (void *Data_ptr);  // add item to end of list.
   };


#endif
