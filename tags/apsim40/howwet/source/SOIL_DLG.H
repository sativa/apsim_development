#if !defined(__soil_dlg_h)              // Sentry, use file only if it's not already included.
#define __soil_dlg_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         soil_dlg.h
    AUTHOR:


    OVERVIEW
    ========
    Class definition for Soil_type_dialog (TDialog).
*/

#include <owl\owlpch.h>
#pragma hdrstop


#include "howwet.rh"            // Definition of all resources.
#include "params.h"
#include <owl\combobox.h>
#include <owl\edit.h>

//{{TDialog = Soil_type_dialog}}
class Soil_type_dialog : public TDialog {
protected:
	TComboBox *Soil_type_control;			//	 pointer to soil type combo box
	TEdit *Soil_depth_control;          // soil depth control
   TStatic *Howwet_sw_control;         // Howwet estimate of soil water
   TEdit *User_sw_control;             // User estimate of soil water
   TEdit *Slope_control;               // Slope control
   TEdit *Age_control;                 // Age of cultivation control
   TStatic *Howwet_orgc_control;       // Howwet estimate of orgc
   TEdit *User_orgc_control;           // User estimate of orgc
   Parameters *Param_ptr;              // pointer to parameters object
   bool User_changed;                  // Has user changed the edit controls?

   int Saved_soil_type;                // Saved soil type
   float Saved_soil_depth;             // Saved soil depth
   float Saved_user_sw;                // Saved user soil water
   float Saved_slope;                  // Saved slope
   float Saved_age;                    // Saved age cultivation
   float Saved_user_orgc;              // Saved user orgc

   void Set_all_fields (void);         // Set the parameter object from all fields on dialog
   void Fill_soil_depth_field (void);  // Fill soil depth field.
   void Fill_soil_water_field (void);  // Fill soil water field.
   void Fill_age_cultivation_field (void);
                                       // Fill age cultivation field.
   void Fill_organic_carbon_field (void);
                                       // Fill organic carbon field.
   void Fill_slope_field (void);       // Fill soil water field.
   void Fill_calc_fields (void);       // Fill all calculated fields.

public:
    Soil_type_dialog (TWindow* parent, Parameters *Param_ptr, TResId resId = ID_SOIL_TYPE_DIALOG, TModule* module = 0);
    virtual ~Soil_type_dialog ();

//{{Soil_type_dialogVIRTUAL_BEGIN}}
public:
    virtual void SetupWindow ();
    virtual bool CanClose ();
    virtual void CloseWindow (int retVal = 0);
//{{Soil_type_dialogVIRTUAL_END}}

//{{Soil_type_dialogRSP_TBL_BEGIN}}
protected:
    void Soil_depth_change ();
    void User_water_change ();
    void Age_cult_change ();
    void User_orgc_change ();
    void Cancel_button ();
    void Help1 ();
    void Help2 ();
    void Help3 ();
    void Soil_type_change ();
//{{Soil_type_dialogRSP_TBL_END}}
DECLARE_RESPONSE_TABLE(Soil_type_dialog);
};    //{{Soil_type_dialog}}


#endif                                      // __soil_dlg_h sentry.

