#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include <cl\transdlg.h>

#include <consts.rh>
#include <assert.h>
DEFINE_RESPONSE_TABLE1(Transfer_dialog, TDialog)
   EV_BN_CLICKED(IDC_PUSHBUTTON1, Button1_pressed),
   EV_BN_CLICKED(IDC_PUSHBUTTON2, Button2_pressed),
END_RESPONSE_TABLE;

// *******************************************************************
       Transfer_dialog::Transfer_dialog
                          (TWindow*                parent,
                           TResId                  templateId,
                           TListBox*               List_box1,
                           TListBox*               List_box2,
                           Transfer_dialog_data*   Transfer_data1,
                           Transfer_dialog_data*   Transfer_data2,
                           TModule*                module)
         : TDialog(parent, templateId, module)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 16/8/95

//  Calls:

//  Assertions

//  Internal variables

// -------------------- Executable code section ----------------------

   Transfer_data1_ptr = Transfer_data1;
   Transfer_data2_ptr = Transfer_data2;

   List_box1_ptr = List_box1;
   List_box2_ptr = List_box2;

   List_box1_ptr->SetParent(this);
   List_box2_ptr->SetParent(this);
   }

// *******************************************************************
       void Transfer_dialog::Transfer_item(int Item_number,
                                           Transfer_dialog_data *Source,
                                           TListBox *Source_list_box,
                                           Transfer_dialog_data *Destination,
                                           TListBox *Destination_list_box)  {
// *******************************************************************

//  Short description:
//    Transfer the specified item from source listbox to destination listbox

//  Notes:

//  Changes:
//    DPH 16/8/95

//  Calls:

//  Assertions

//  Internal variables

// -------------------- Executable code section ----------------------

   if (Item_number >= 0)
      {
      void *Item = Source->Get_item(Item_number);

      // add item to new destination

      Destination->Add_item(Item);
      Destination_list_box->AddString("");

      // remove item from source.

      Source->Remove_item(Item_number);
      Source_list_box->DeleteString(Item_number);
      }
   }

// *******************************************************************
       void Transfer_dialog::Button1_pressed(void)  {
// *******************************************************************

//  Short description:
//    button1 was pressed.

//  Notes:

//  Changes:
//    DPH 16/8/95

//  Calls:

//  Assertions

//  Internal variables

// -------------------- Executable code section ----------------------

   Transfer_item(List_box1_ptr->GetSelIndex(),
                 Transfer_data1_ptr,
                 List_box1_ptr,
                 Transfer_data2_ptr,
                 List_box2_ptr);
   }

// *******************************************************************
       void Transfer_dialog::Button2_pressed(void)  {
// *******************************************************************

//  Short description:
//    button2 was pressed.

//  Notes:

//  Changes:
//    DPH 16/8/95

//  Calls:

//  Assertions

//  Internal variables

// -------------------- Executable code section ----------------------

   Transfer_item(List_box2_ptr->GetSelIndex(),
                 Transfer_data2_ptr,
                 List_box2_ptr,
                 Transfer_data1_ptr,
                 List_box1_ptr);
   }

