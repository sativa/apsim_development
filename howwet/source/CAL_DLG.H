#if !defined(__cal_dlg_h)              // Sentry, use file only if it's not already included.
#define __cal_dlg_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         cal_dlg.h
    AUTHOR:       


    OVERVIEW
    ========
    Class definition for Calender_dialog (TDialog).      
*/

#include <owl\owlpch.h>
#pragma hdrstop


#include "howwet.rh"            // Definition of all resources.
#include <cl\cldrcont.h>

#include "params.h"
#include <owl\static.h>

class RainfallFile;

//{{TDialog = Calender_dialog}}
class Calender_dialog : public TDialog {
protected:
	Parameters *Param_ptr;					// pointer to parameters object
   RainfallFile* rainfallFile;

   TStatic *Period_control;            // pointer to period static control
   TStatic *Rain_title_control;        // pointer to rainfall title control
   Calender Cal;                       // our calender.
   Calender_control *Calender_cont;    // pointer to calender control
	String_array *Data_array;				// array to pass to calender

	void Setup_calender(void);				// setup the calender
public:
    Calender_dialog (TWindow* parent,
                     Parameters *Param_ptr,
                     RainfallFile* rainfallFile,
                     TResId resId = IDC_CALENDER_DLG,
                     TModule* module = 0);
    virtual ~Calender_dialog ();

//{{Calender_dialogVIRTUAL_BEGIN}}
public:
    virtual void SetupWindow ();
//{{Calender_dialogVIRTUAL_END}}

//{{Calender_dialogRSP_TBL_BEGIN}}
protected:
    void Help1 ();
    void Print_button ();
//{{Calender_dialogRSP_TBL_END}}
DECLARE_RESPONSE_TABLE(Calender_dialog);
};    //{{Calender_dialog}}


#endif                                      // __cal_dlg_h sentry.

