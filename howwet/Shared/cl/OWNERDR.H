#if !defined(__OWNERDR_H)
#define __OWNERDR_H

// *******************************************************************
       class huge GOwner_draw_base  {
// *******************************************************************

//  Short description:
//    Class for encapsulating an owner draw object.

   protected:
      int Selected_item;               // Selected item.
      int Num_items;                   // number of items in list.
 
   public:
      GOwner_draw_base(void);          // constructor
      void Set_selected_item(int Item_number)
         {
         Selected_item = Item_number;
         };                            // set the selected item.
      int Get_selected_item (void)
         {
         return Selected_item;
         };                            // return the selected item.
      int Get_num_items (void)
         {
         return Num_items;
         };                            // return number of items in list.

      virtual void DrawItem(DRAWITEMSTRUCT far &Draw_info)
         {};                           // Draw item
   };

#include <owl\combobox.h>
// *******************************************************************
       class huge  GOwner_draw_combo : public TComboBox {
// *******************************************************************

//  Short description:
//      Windows combo box that has been subclassed to display owner drawn
//      elements.

//  Notes:

//  Changes:
//      DPH 25/5/94

// ------------------------ Declaration section -------------------------

   protected :

//   Protected data
      GOwner_draw_base* Owner_draw_ptr; // pointer to owner draw combo box.

//   Protected member functions
      void DrawItem(DRAWITEMSTRUCT far &Draw_info);
                                       // Draw list box item
      void SetupWindow(void);          // Setup our combo box.
      bool CanClose(void);             // Allow closure of control?

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      GOwner_draw_combo (TWindow*          Parent,
                         int               resID,
                         TModule*          module,
                         GOwner_draw_base* Owner_draw_ptr);
                                       // constructor
   };


// *******************************************************************
       class huge  GOwner_draw_list : public TListBox {
// *******************************************************************

//  Short description:
//      Windows list box that has been subclassed to display an owner drawn
//      list.

//  Notes:

//  Changes:
//      DPH 25/5/94

// --------------------- Declaration section -------------------------

   protected :

//   Protected data
      GOwner_draw_base* Owner_draw_ptr; // pointer to owner draw combo box.

//   Protected member functions
      void DrawItem(DRAWITEMSTRUCT far &Draw_info);
                                       // Draw list box item
      void SetupWindow(void);          // Setup our combo box.
      bool CanClose(void);             // Allow closure of control?

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      GOwner_draw_list  (TWindow*          Parent,
                         int               resID,
                         TModule*          module,
                         GOwner_draw_base* Owner_draw_ptr);
                                       // constructor
   };

#endif
