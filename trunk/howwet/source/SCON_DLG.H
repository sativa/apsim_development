#if !defined(__scon_dlg_h)              // Sentry, use file only if it's not already included.
#define __scon_dlg_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         scon_dlg.h
    AUTHOR:       


    OVERVIEW
    ========
    Class definition for Soil_conditions_dlg (TDialog).
*/

#include <owl\owlpch.h>
#pragma hdrstop


#include "howwet.rh"            // Definition of all resources.

#include <owl\edit.h>

class RainfallFile;

//{{TDialog = Soil_conditions_dlg}}
class Soil_conditions_dlg : public TDialog {
protected:
   TEdit *Start_date_control;          // Start date control
   TEdit *Perc_full_control;           // Percent full control
   TEdit *mm_full_control;             // mm full control
   TEdit *Start_cover_control;         // starting cover control
   TEdit *End_date_control;            // End date control
   TEdit *End_cover_control;           // Ending cover control
   TStatic *PAWC_control;              // PAWC control
   TStatic* Met_range_label;
   Parameters *Param_ptr;              // Pointer to parameters object
   RainfallFile* rainfallFile;
   void Set_pawc_static (void);			// Set the value of the PAWC static control

public:
    Soil_conditions_dlg (TWindow* parent, Parameters *Param_ptr, RainfallFile* rainfallFile,
                         TResId resId = IDC_SOIL_CONDITIONS, TModule* module = 0);
    virtual ~Soil_conditions_dlg ();

//{{Soil_conditions_dlgVIRTUAL_BEGIN}}
public:
    virtual void SetupWindow ();
    virtual bool CanClose ();
//{{Soil_conditions_dlgVIRTUAL_END}}

//{{Soil_conditions_dlgRSP_TBL_BEGIN}}
protected:
    void Perc_full_change ();
    void mm_full_change ();
    void Help1 ();
    void Help2 ();
//{{Soil_conditions_dlgRSP_TBL_END}}
DECLARE_RESPONSE_TABLE(Soil_conditions_dlg);
};    //{{Soil_conditions_dlg}}

#endif                                      // __scon_dlg_h sentry.

