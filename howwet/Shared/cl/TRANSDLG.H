#if !defined(__TRANSDLG_H)
#define __TRANSDLG_H

#include <owl\dialog.h>
#include <owl\listbox.h>
// *******************************************************************
       class huge  Transfer_dialog_data  {
// *******************************************************************

//  Short description:
//    Object for encapsulating a dialog box that displays 2 listbox
//    controls and allows user to transfer items from list 1 to list 2.

//  Notes:

//  Changes:
//    DPH 21/7/95

   public:

   virtual void* Get_item (int Item_id)
      {return NULL;};                  // return pointer to item
   virtual void Remove_item (int Item_id)
      {};                              // remove item from list.
   virtual void Add_item (void *Data_ptr)
      {};                              // add item to end of list.

   };

// *******************************************************************
       class huge Transfer_dialog : public TDialog   {
// *******************************************************************

//  Short description:
//    Object for encapsulating a dialog box that displays 2 listbox
//    controls and allows user to transfer items from list 1 to list 2.
//    Both list boxes passed into the constructor must be derived
//    from Transfer_dialog_data.

//  Notes:

//  Changes:
//    DPH 21/7/95

   protected:
      TListBox *List_box1_ptr;         // pointer to 1st listbox.
      TListBox *List_box2_ptr;         // pointer to 2nd listbox.
      Transfer_dialog_data *Transfer_data1_ptr;
                                       // pointer to 1st transfer data
      Transfer_dialog_data *Transfer_data2_ptr;
                                       // pointer to 2nd transfer data
      void Button1_pressed(void);      // button1 was pressed.
      void Button2_pressed(void);      // button2 was pressed.
      void Transfer_item(int Item_number,
                         Transfer_dialog_data*  Source,
                         TListBox*              Source_list_box,
                         Transfer_dialog_data*  Destination,
                         TListBox*              Destination_list_box);
                                       // Transfer the specified item from source
                                       // to destination list boxes.

   public:
      Transfer_dialog(TWindow*                  parent,
                      TResId                    templateId,
                      TListBox*                 List_box1,
                      TListBox*                 List_box2,
                      Transfer_dialog_data*     Transfer_data1,
                      Transfer_dialog_data*     Transfer_data2,
                      TModule*                  module = 0);
                                       // constructor.
   DECLARE_RESPONSE_TABLE(Transfer_dialog);
   };

#endif
