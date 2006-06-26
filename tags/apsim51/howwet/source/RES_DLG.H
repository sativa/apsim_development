#if !defined(__res_dlg_h)              // Sentry, use file only if it's not already included.
#define __res_dlg_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         res_dlg.h
    AUTHOR:       


    OVERVIEW
    ========
    Class definition for Results_dialog (TDialog).      
*/

#include <owl\owlpch.h>
#pragma hdrstop


#include "howwet.rh"            // Definition of all resources.
#include "params.h"
#include "simul.h"
#include <owl\static.h>

class RainfallFile;

//{{TDialog = Results_dialog}}
class Results_dialog : public TDialog {
protected:
   Parameters *Param_ptr;              // pointer to parameters object
   Simul *Simul_ptr;                   // pointer to simulation object
   TStatic *Period_control;            // Period control
   TStatic *Soil_type_control;         // Soil type control
   TStatic *Cover_control;             // Cover control
   TStatic *Start_water_mm_control;    // Starting soil water control mm
   TStatic *Start_water_perc_control;  // Starting soil water control percent
   TStatic *Final_water_mm_control;    // Final soil water control mm
   TStatic *Final_water_perc_control;  // Final soil water control percent
   TStatic *Fallow_rain_control;       // Fallow rainfall control
   TStatic *Evaporation_mm_control;    // Evaporation control mm
   TStatic *Evaporation_perc_control;  // Evaporation control percent
   TStatic *Runoff_mm_control;         // Runoff control mm
   TStatic *Runoff_perc_control;       // Runoff control percent
   TStatic *Drainage_mm_control;       // Drainage control mm
   TStatic *Drainage_perc_control;     // Drainage control percent
   TStatic *Gain_water_mm_control;     // Gain in soil water control mm
   TStatic *Gain_water_perc_control;   // Gain in soil water control perc
   TStatic *Gain_nitrate_control;      // Gain in soil nitrate control
   TStatic *mm1_control;					// label 'mm'
   TStatic *perc1_control;             // label '%'
   TStatic *mm2_control;               // label 'mm'
   TStatic *perc2_control;             // label '%'
   TStatic *Fallow_eff_control;        // label 'fallow efficiency'
	TFont* Bold_font;							// bold font.
   RainfallFile* rainfallFile;

public:
    Results_dialog (TWindow* parent,
                    Parameters *Param_ptr,
                    Simul *Simul_ptr,
                    RainfallFile* rainfallFile,
                    TResId resId = IDC_RESULT_TABLE,
                    TModule* module = 0);
    virtual ~Results_dialog ();

//{{Results_dialogVIRTUAL_BEGIN}}
public:
    virtual void SetupWindow ();
//{{Results_dialogVIRTUAL_END}}

//{{Results_dialogRSP_TBL_BEGIN}}
protected:
    void Print_button ();
    void Help1 ();
    void Help2 ();
    HBRUSH EvCtlColor (HDC dc, HWND hWndChild, uint ctlType);
//{{Results_dialogRSP_TBL_END}}
DECLARE_RESPONSE_TABLE(Results_dialog);
};    //{{Results_dialog}}


#endif                                      // __res_dlg_h sentry.

