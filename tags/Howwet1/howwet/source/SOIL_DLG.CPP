/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         soil_dlg.cpp
    AUTHOR:       


    OVERVIEW
    ========
    Source file for implementation of Soil_type_dialog (TDialog).
*/

#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include "soil_dlg.h"

#include <consts.rh>
#include <stdio.h>
#include "so_what.h"

//
// Build a response table for all messages/commands handled
// by the application.
//
DEFINE_RESPONSE_TABLE1(Soil_type_dialog, TDialog)
//{{Soil_type_dialogRSP_TBL_BEGIN}}
    EV_EN_CHANGE(IDC_EDIT1, Soil_depth_change),
    EV_EN_CHANGE(IDC_EDIT2, User_water_change),
    EV_EN_CHANGE(IDC_EDIT4, Age_cult_change),
    EV_EN_CHANGE(IDC_EDIT5, User_orgc_change),
    EV_BN_CLICKED(IDCANCEL, Cancel_button),
    EV_BN_CLICKED(INFO1, Help1),
    EV_BN_CLICKED(INFO2, Help2),
    EV_BN_CLICKED(INFO3, Help3),
    EV_CBN_SELCHANGE(IDC_COMBOBOX1, Soil_type_change),
//{{Soil_type_dialogRSP_TBL_END}}
END_RESPONSE_TABLE;


//{{Soil_type_dialog Implementation}}


Soil_type_dialog::Soil_type_dialog (TWindow* parent, Parameters *Param_p,
                                    TResId resId, TModule* module):
    TDialog(parent, resId, module)
{
    // INSERT>> Your constructor code here.

	Soil_type_control = new TComboBox(this, IDC_COMBOBOX1);
	Soil_depth_control = new TEdit(this, IDC_EDIT1);
   Howwet_sw_control = new TStatic(this, IDC_STATIC1);
   User_sw_control = new TEdit(this, IDC_EDIT2);
   Slope_control = new TEdit(this, IDC_EDIT3);
   Age_control = new TEdit(this, IDC_EDIT4);
   Howwet_orgc_control = new TStatic(this, IDC_STATIC2);
   User_orgc_control = new TEdit(this, IDC_EDIT5);

   Param_ptr = Param_p;

   // Save all current settings so we can restore them if need be.

   Saved_soil_type = Param_ptr->Get_current_soil();
   Saved_soil_depth = Param_ptr->Get_user_soil_depth();
   Saved_user_sw = Param_ptr->Get_user_soil_water_max();
   Saved_slope = Param_ptr->Get_user_slope();
   Saved_age = Param_ptr->Get_user_age_cultivation();
   Saved_user_orgc = Param_ptr->Get_user_organic_carbon();
}


Soil_type_dialog::~Soil_type_dialog ()
{
    Destroy();

    // INSERT>> Your destructor code here.

}


// *******************************************************************
      void Soil_type_dialog::SetupWindow ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
      String_array *Soils;             // Soil descriptions

// -------------------- Executable code section ----------------------

    TDialog::SetupWindow();

   // INSERT>> Your code here.

   User_changed = FALSE;

   // Get soil descriptions

   Param_ptr->Get_soil_list(&Soils);

   // Fill transfer structure

   for (int Index = 0;
        Index < Soils->GetItemsInContainer();
        Index++)
      {
      Soil_type_control->AddString((*Soils)[Index].c_str());
      }
   Soil_type_control->SetSelIndex(Param_ptr->Get_current_soil());

   Fill_soil_depth_field ();
   Fill_soil_water_field ();
   Fill_age_cultivation_field ();
   Fill_organic_carbon_field ();
   Fill_slope_field ();
   Fill_calc_fields ();

   User_changed = TRUE;
   }

// *******************************************************************
      void Soil_type_dialog::Fill_soil_depth_field (void)  {
// *******************************************************************

//  Short description:
//    Fill soil depth field.

//  Notes:

//  Changes:
//    DPH 24/10/95

//  Internal variables
      char St[50];                     // String

// -------------------- Executable code section ----------------------

   if (Param_ptr->Get_user_soil_depth() != 0.0)
      sprintf(St, "%4.1f", Param_ptr->Get_user_soil_depth());
   else
      strcpy(St, "");

   Soil_depth_control->SetText(St);
   }

// *******************************************************************
      void Soil_type_dialog::Fill_soil_water_field (void)  {
// *******************************************************************

//  Short description:
//    Fill soil water field

//  Notes:

//  Changes:
//    DPH 24/10/95

//  Internal variables
      char St[50];                     // String

// -------------------- Executable code section ----------------------

   if (Param_ptr->Get_user_soil_water_max() != 0.0)
      sprintf(St, "%4.0f", Param_ptr->Get_user_soil_water_max());
   else
      strcpy(St, "");
   User_sw_control->SetText(St);
   }

// *******************************************************************
      void Soil_type_dialog::Fill_age_cultivation_field (void)  {
// *******************************************************************

//  Short description:
//    Fill age cultivation field.

//  Notes:

//  Changes:
//    DPH 24/10/95

//  Internal variables
      char St[50];                     // String

// -------------------- Executable code section ----------------------

   if (Param_ptr->Get_user_age_cultivation() != 0.0)
      sprintf(St, "%4.0f", Param_ptr->Get_user_age_cultivation());
   else
      strcpy(St, "");
   Age_control->SetText(St);
   }

// *******************************************************************
      void Soil_type_dialog::Fill_organic_carbon_field (void)  {
// *******************************************************************

//  Short description:
//    Fill organic carbon field.

//  Notes:

//  Changes:
//    DPH 24/10/95

//  Internal variables
      char St[50];                     // String

// -------------------- Executable code section ----------------------

   if (Param_ptr->Get_user_organic_carbon() != 0.0)
      sprintf(St, "%4.1f", Param_ptr->Get_user_organic_carbon());
   else
      strcpy(St, "");
   User_orgc_control->SetText(St);
   }

// *******************************************************************
      void Soil_type_dialog::Fill_slope_field (void)  {
// *******************************************************************

//  Short description:
//    Fill slope field.

//  Notes:

//  Changes:
//    DPH 24/10/95

//  Internal variables
      char St[50];                     // String

// -------------------- Executable code section ----------------------

   sprintf(St, "%4.1f", Param_ptr->Get_user_slope());
   Slope_control->SetText(St);
   }

// *******************************************************************
      void Soil_type_dialog::Fill_calc_fields (void)  {
// *******************************************************************

//  Short description:
//    Fill all calculated fields in dialog box.

//  Notes:

//  Changes:
//    DPH 24/10/95

//  Internal variables
      char St[50];                     // String

// -------------------- Executable code section ----------------------

   // Howwet soil water estimate

   sprintf(St, "%4.0f", Param_ptr->Get_calc_soil_water_max());
   Howwet_sw_control->SetText(St);

   // Howwet orgc estimate

   sprintf(St, "%4.1f", Param_ptr->Get_calc_organic_carbon());
   Howwet_orgc_control->SetText(St);
   }

// *******************************************************************
      void Soil_type_dialog::Set_all_fields (void)  {
// *******************************************************************

//  Short description:
//    Set the parameter object from all fields on dialog

//  Notes:

//  Changes:
//    DPH 24/10/95

//  Internal variables
      char St[50];                     // Char String
      GString Str;                     // String object

// -------------------- Executable code section ----------------------

   // Soil type

   Param_ptr->Set_current_soil(Soil_type_control->GetSelIndex());

   // Soil depth

   Soil_depth_control->GetText(St, sizeof(St));
   if (strlen(St) > 0)
      {
      Str = St;
      Param_ptr->Set_soil_depth(Str.Get_real());
      }

   // User soil water estimate

   User_sw_control->GetText(St, sizeof(St));
   if (strlen(St) > 0)
      {
      Str = St;
      Param_ptr->Set_soil_water_max(Str.Get_real());
      }

   // Slope

   Slope_control->GetText(St, sizeof(St));
   Str = St;
   Param_ptr->Set_slope(Str.Get_real());

   // Age cultivation

   Age_control->GetText(St, sizeof(St));
   if (strlen(St) > 0)
      {
      Str = St;
      Param_ptr->Set_age_cultivation(Str.Get_real());
      }

   // User orgc estimate

   User_orgc_control->GetText(St, sizeof(St));
   if (strlen(St) > 0)
      {
      Str = St;
      Param_ptr->Set_organic_carbon(Str.Get_real());
      }
   }

// *******************************************************************
      void Soil_type_dialog::Soil_depth_change (void)  {
// *******************************************************************

//  Short description:
//    The soil depth has changed

//  Notes:

//  Changes:
//    DPH

//  Internal variables
      char St[50];                     // Value of control

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   Soil_depth_control->GetText(St, sizeof(St));
   if (User_changed && strlen(St) > 0)
      {
      User_sw_control->SetText("");
      Set_all_fields();
      Fill_calc_fields();
      }
   }


// *******************************************************************
      void Soil_type_dialog::User_water_change (void)  {
// *******************************************************************

//  Short description:
//    User estimate of water has changed

//  Notes:

//  Changes:
//    DPH

//  Internal variables
      char St[50];                     // Value of control

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   User_sw_control->GetText(St, sizeof(St));
   if (User_changed && strlen(St) > 0)
      {
      Soil_depth_control->SetText("");
      Set_all_fields();
      Fill_calc_fields();
      }
   }


// *******************************************************************
      void Soil_type_dialog::Age_cult_change (void)  {
// *******************************************************************

//  Short description:
//    Age of cultivation has changed

//  Notes:

//  Changes:
//    DPH

//  Internal variables
      char St[50];                     // Value of control

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   Age_control->GetText(St, sizeof(St));
   if (User_changed && strlen(St) > 0)
      {
      User_orgc_control->SetText("");
      Set_all_fields();
      Fill_calc_fields();
      }
   }

// *******************************************************************
      void Soil_type_dialog::User_orgc_change (void)  {
// *******************************************************************

//  Short description:
//    User organic carbon has changed

//  Notes:

//  Changes:
//    DPH

//  Internal variables
      char St[50];                     // Value of control

// -------------------- Executable code section ----------------------

   // INSERT>> Your code here.

   User_orgc_control->GetText(St, sizeof(St));
   if (User_changed && strlen(St) > 0)
      {
      Age_control->SetText("");
      Set_all_fields();
      Fill_calc_fields();
      }
   }


// *******************************************************************
      bool Soil_type_dialog::CanClose ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
      char St1[50];                    // string 1
      char St2[50];                    // string 2
      GString Str;                     // string object
      float Number;                    // real number

// -------------------- Executable code section ----------------------

    TDialog::CanClose();

   // INSERT>> Your code here.

	// Check soil water storage capacity

   Soil_depth_control->GetText(St1, sizeof(St1));
   User_sw_control->GetText(St2, sizeof(St2));

	if (strlen(St1) == 0 && strlen(St2) == 0)
      {
		MessageBox("You haven't entered a soil depth or an estimate of soil water",
					  "Error", MB_ICONSTOP | MB_OK);
      return FALSE;
      }

   if (strlen(St2) > 0)
		{
      Str = St2;
		Number = Str.Get_real();
		if (Number < 10 || Number > 250)
         {
			MessageBox("Soil water capacity must be between 10 and 250mm",
						  "Error", MB_ICONSTOP | MB_OK);
         return FALSE;
         }
		}

	// Check soil depth

	if (strlen(St1) > 0)
		{
      float *Depths;
      Param_ptr->Get_ini_soil_depths(&Depths);
      float Depth_2_layers = (Depths[0] + Depths[1]) / 100;

      Str = St1;
		Number = Str.Get_real();
		if (Number < Depth_2_layers || Number > 2.5)
         {
         sprintf(St1, "Soil depth must be between %3.1fm and 2.5m", Depth_2_layers);
			MessageBox(St1,
						  "Error", MB_ICONSTOP | MB_OK);
         return FALSE;
         }
      }

	// check to make sure user entered something into Nitrogen

   Age_control->GetText(St1, sizeof(St1));
   User_orgc_control->GetText(St2, sizeof(St2));
	if (strlen(St1) == 0 && strlen(St2) == 0)
		{
		MessageBox("You haven''t specified the number of years of cultivation OR"
					  " an estimate of organic carbon content",
					  "Error", MB_ICONSTOP | MB_OK);
      return FALSE;
		}

	// Check cultivation (yrs)

   Str = St1;
   Number = Str.Get_real();
	if (Number < 0 || Number > 200)
		{
		MessageBox("Invalid age of cultivation", "Error", MB_ICONSTOP | MB_OK);
      return FALSE;
		}

	// Check organic carbon content (%)

   Str = St2;
   Number = Str.Get_real();
   if (Number < 0 || Number > 100)
		{
		MessageBox("Invalid organic carbon content", "Error", MB_ICONSTOP | MB_OK);
      return FALSE;
		}

	// Check slope (%)

   Age_control->GetText(St1, sizeof(St1));
   Str = St1;
   Number = Str.Get_real();
   if (Number < 0 || Number > 100)
		{
		MessageBox("Invalid slope", "Error", MB_ICONSTOP | MB_OK);
      return FALSE;
		}
   else
      {
      Set_all_fields();
      return TRUE;
      }
   }

// *******************************************************************
      void Soil_type_dialog::CloseWindow (int retVal)  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   if (retVal == IDCANCEL)
      {
      Param_ptr->Set_current_soil(Saved_soil_type);
      if (Saved_soil_depth != 0.0)
         Param_ptr->Set_soil_depth(Saved_soil_depth);
      else
         Param_ptr->Set_soil_water_max(Saved_user_sw);

      Param_ptr->Set_slope(Saved_slope);

      if (Saved_age != 0.0)
         Param_ptr->Set_age_cultivation(Saved_age);
      else
         Param_ptr->Set_organic_carbon(Saved_user_orgc);
      }

    TDialog::CloseWindow(retVal);
   }


// *******************************************************************
      void Soil_type_dialog::Cancel_button ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   Param_ptr->Set_current_soil(Saved_soil_type);
   if (Saved_soil_depth != 0.0)
      Param_ptr->Set_soil_depth(Saved_soil_depth);
   else
      Param_ptr->Set_soil_water_max(Saved_user_sw);

   Param_ptr->Set_slope(Saved_slope);

   if (Saved_age != 0.0)
      Param_ptr->Set_age_cultivation(Saved_age);
   else
      Param_ptr->Set_organic_carbon(Saved_user_orgc);

   Destroy(IDCANCEL);
   }


// *******************************************************************
      void Soil_type_dialog::Help1 ()  {
// *******************************************************************

//  Short description:
//    Help button 1 was clicked.

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   So_what_dlg(this, 1, 1).Execute();

   }


// *******************************************************************
      void Soil_type_dialog::Help2 ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   So_what_dlg(this, 1, 2).Execute();
   }


// *******************************************************************
      void Soil_type_dialog::Help3 ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   So_what_dlg(this, 1, 3).Execute();
   }


// *******************************************************************
      void Soil_type_dialog::Soil_type_change ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   User_sw_control->SetText("");
   Set_all_fields();
   Fill_calc_fields();
   }

