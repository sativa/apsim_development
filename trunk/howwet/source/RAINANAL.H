#if !defined(__rainanal_h)              // Sentry, use file only if it's not already included.
#define __rainanal_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         rainanal.h
    AUTHOR:


    OVERVIEW
    ========
    Class definition for Rain_analysis_dlg (TDialog).
*/

#include <owl\owlpch.h>
#pragma hdrstop


#include "howwet.rh"            // Definition of all resources.
#include "params.h"
#include "simul.h"
#include <owl\static.h>

class RainfallFile;

//{{TDialog = Rain_analysis_dlg}}
class Rain_analysis_dlg : public TDialog {
protected:
    TStatic *Period_control;           // pointer to period static
    TStatic *mm15_control;             // < 15 mm control
    TStatic *mm15_50_control;          // 15 - 50 mm control
    TStatic *mm50_control;             // > 50 mm control
    TStatic *Total_control;            // Total for period
    TStatic *perc15_control;           // < 15% control
    TStatic *perc15_50_control;        // 15 - 50 perc control
    TStatic *perc50_control;           // > 50 perc control
    TStatic *Long_term_av_control;     // long term average control
    TStatic *Long_term_perc_control;   // Percent of long term control
    TStatic *Expected_rain1_control;   // Expected rain for next 1 month
    TStatic *Expected_rain2_control;   // Expected rain for next 2 months
    TStatic *Current_sw_control;       // current soil water

    Parameters *Param_ptr;             // Pointer to parameters object
    Simul *Simul_ptr;                  // Pointer to simulation object
    RainfallFile* rainfallFile;

public:
    Rain_analysis_dlg (TWindow* parent,
                       Parameters *Param_ptr,
                       Simul *Simul_ptr,
                       RainfallFile* rainfallFile,
                       TResId resId = IDC_RAINANAL_DLG,
                       TModule* module = 0);
    virtual ~Rain_analysis_dlg ();

//{{Rain_analysis_dlgVIRTUAL_BEGIN}}
public:
    virtual void SetupWindow ();
//{{Rain_analysis_dlgVIRTUAL_END}}

//{{Rain_analysis_dlgRSP_TBL_BEGIN}}
protected:
    void Help1 ();
    void Help2 ();
    void Print_button ();
//{{Rain_analysis_dlgRSP_TBL_END}}
DECLARE_RESPONSE_TABLE(Rain_analysis_dlg);
};    //{{Rain_analysis_dlg}}


#endif                                      // __rainanal_h sentry.

